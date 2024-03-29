{-|
Module      : Z.Data.Vector.FlatIntMap
Description : Fast int map based on sorted vector
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides a simple int key value map based on sorted vector and binary search. It's particularly
suitable for small sized key value collections such as deserializing intermediate representation.
But can also used in various place where insertion and deletion is rare but require fast lookup.

-}

module Z.Data.Vector.FlatIntMap
  ( -- * FlatIntMap backed by sorted vector
    FlatIntMap, sortedKeyValues, size, null, empty, map', imap'
  , pack, packN, packR, packRN
  , unpack, unpackR, packVector, packVectorR
  , lookup
  , delete
  , insert
  , adjust'
  , merge, mergeWithKey'
    -- * fold and traverse
  , foldrWithKey, foldrWithKey', foldlWithKey, foldlWithKey', traverseWithKey
    -- * binary search on vectors
  , binarySearch
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Foldable              as Foldable
import qualified Data.Traversable           as Traversable
import qualified Data.Semigroup             as Semigroup
import qualified Data.Monoid                as Monoid
import qualified Data.Primitive.SmallArray  as A
import qualified Z.Data.Vector.Base         as V
import qualified Z.Data.Vector.Extra        as V
import qualified Z.Data.Vector.Sort         as V
import qualified Z.Data.Text.Print          as T
import           Data.Function              (on)
import           Data.Bits                  (unsafeShiftR)
import           Data.Data
import           Prelude hiding (lookup, null)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

--------------------------------------------------------------------------------

newtype FlatIntMap v = FlatIntMap { sortedKeyValues :: V.Vector (V.IPair v) }
    deriving (Show, Eq, Ord, Typeable)

instance T.Print v => T.Print (FlatIntMap v) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP p (FlatIntMap vec) = T.parenWhen (p > 10) $ do
        "FlatIntMap{"
        T.intercalateVec T.comma (\ (V.IPair i v) ->
            T.toUTF8Builder i >> T.char7 ':' >> T.toUTF8Builder v) vec
        T.char7 '}'

instance (Arbitrary v) => Arbitrary (FlatIntMap v) where
    arbitrary = pack <$> arbitrary
    shrink v = pack <$> shrink (unpack v)

instance (CoArbitrary v) => CoArbitrary (FlatIntMap v) where
    coarbitrary = coarbitrary . unpack

instance Semigroup.Semigroup (FlatIntMap v) where
    {-# INLINE (<>) #-}
    (<>) = merge

instance Monoid.Monoid (FlatIntMap v) where
    {-# INLINE mappend #-}
    mappend = (<>)
    {-# INLINE mempty #-}
    mempty = empty

instance NFData v => NFData (FlatIntMap v) where
    {-# INLINE rnf #-}
    rnf (FlatIntMap ivs) = rnf ivs

instance Functor (FlatIntMap) where
    {-# INLINE fmap #-}
    fmap f (FlatIntMap vs) = FlatIntMap (V.map' (fmap f) vs)

instance Foldable.Foldable FlatIntMap where
    {-# INLINE foldr' #-}
    foldr' f = foldrWithKey' (const f)
    {-# INLINE foldr #-}
    foldr f = foldrWithKey (const f)
    {-# INLINE foldl' #-}
    foldl' f = foldlWithKey' (\ a _ v -> f a v)
    {-# INLINE foldl #-}
    foldl f = foldlWithKey (\ a _ v -> f a v)
    {-# INLINE toList #-}
    toList = fmap V.isnd . unpack
    {-# INLINE null #-}
    null (FlatIntMap vs) = V.null vs
    {-# INLINE length #-}
    length (FlatIntMap vs) = V.length vs
    {-# INLINE elem #-}
    elem a (FlatIntMap vs) = elem a (map V.isnd $ V.unpack vs)

instance Traversable.Traversable FlatIntMap where
    {-# INLINE traverse #-}
    traverse f = traverseWithKey (const f)

size :: FlatIntMap v -> Int
{-# INLINE size #-}
size = V.length . sortedKeyValues

null :: FlatIntMap v -> Bool
{-# INLINE null #-}
null = V.null . sortedKeyValues

map' :: (v -> v') -> FlatIntMap v -> FlatIntMap v'
{-# INLINE map' #-}
map' f (FlatIntMap vs) = FlatIntMap (V.map' (V.mapIPair' f) vs)

imap' :: (Int -> v -> v') -> FlatIntMap v -> FlatIntMap v'
{-# INLINE imap' #-}
imap' f (FlatIntMap vs) = FlatIntMap (V.imap' (\ i -> V.mapIPair' (f i)) vs)

-- | /O(1)/ empty flat map.
empty :: FlatIntMap v
{-# NOINLINE empty #-}
empty = FlatIntMap V.empty

-- | /O(N*logN)/ Pack list of key values, on key duplication prefer left one.
pack :: [V.IPair v] -> FlatIntMap v
{-# INLINABLE pack #-}
pack kvs = FlatIntMap (V.mergeDupAdjacentLeft ((==) `on` V.ifst) (V.mergeSortBy (compare `on` V.ifst) (V.pack kvs)))

-- | /O(N*logN)/ Pack list of key values with suggested size, on key duplication prefer left one.
packN :: Int -> [V.IPair v] -> FlatIntMap v
{-# INLINABLE packN #-}
packN n kvs = FlatIntMap (V.mergeDupAdjacentLeft ((==) `on` V.ifst) (V.mergeSortBy (compare `on` V.ifst) (V.packN n kvs)))

-- | /O(N*logN)/ Pack list of key values, on key duplication prefer right one.
packR :: [V.IPair v] -> FlatIntMap v
{-# INLINABLE packR #-}
packR kvs = FlatIntMap (V.mergeDupAdjacentRight ((==) `on` V.ifst) (V.mergeSortBy (compare `on` V.ifst) (V.pack kvs)))

-- | /O(N*logN)/ Pack list of key values with suggested size, on key duplication prefer right one.
packRN :: Int -> [V.IPair v] -> FlatIntMap v
{-# INLINABLE packRN #-}
packRN n kvs = FlatIntMap (V.mergeDupAdjacentRight ((==) `on` V.ifst) (V.mergeSortBy (compare `on` V.ifst) (V.packN n kvs)))

-- | /O(N)/ Unpack key value pairs to a list sorted by keys in ascending order.
--
-- This function works with @foldr/build@ fusion in base.
unpack :: FlatIntMap v -> [V.IPair v]
{-# INLINE unpack #-}
unpack = V.unpack . sortedKeyValues

-- | /O(N)/ Unpack key value pairs to a list sorted by keys in descending order.
--
-- This function works with @foldr/build@ fusion in base.
unpackR :: FlatIntMap v -> [V.IPair v]
{-# INLINE unpackR #-}
unpackR = V.unpackR . sortedKeyValues

-- | /O(N*logN)/ Pack vector of key values, on key duplication prefer left one.
packVector :: V.Vector (V.IPair v) -> FlatIntMap v
{-# INLINABLE packVector #-}
packVector kvs = FlatIntMap (V.mergeDupAdjacentLeft ((==) `on` V.ifst) (V.mergeSortBy (compare `on` V.ifst) kvs))

-- | /O(N*logN)/ Pack vector of key values, on key duplication prefer right one.
packVectorR :: V.Vector (V.IPair v) -> FlatIntMap v
{-# INLINABLE packVectorR #-}
packVectorR kvs = FlatIntMap (V.mergeDupAdjacentRight ((==) `on` V.ifst) (V.mergeSortBy (compare `on` V.ifst) kvs))

-- | /O(logN)/ Binary search on flat map.
lookup :: Int -> FlatIntMap v -> Maybe v
{-# INLINABLE lookup #-}
lookup _ (FlatIntMap (V.Vector _ _ 0)) = Nothing
lookup k' (FlatIntMap (V.Vector arr s0 l)) = go s0 (s0+l-1)
  where
    go !s !e
        | s == e =
            case arr `A.indexSmallArray` s of (V.IPair k v) | k == k'  -> Just v
                                                            | otherwise -> Nothing
        | s >  e = Nothing
        | otherwise =
            let mid = (s+e) `unsafeShiftR` 1
                (V.IPair k v)  = arr `A.indexSmallArray` mid
            in case k' `compare` k of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Just v

-- | /O(N)/ Insert new key value into map, replace old one if key exists.
insert :: Int -> v -> FlatIntMap v -> FlatIntMap v
{-# INLINABLE insert #-}
insert k v (FlatIntMap vec) =
    case binarySearch vec k of
        Left i -> FlatIntMap (V.unsafeInsertIndex vec i (V.IPair k v))
        Right i -> FlatIntMap (V.unsafeModifyIndex vec i (const (V.IPair k v)))

-- | /O(N)/ Delete a key value pair by key.
delete :: Int -> FlatIntMap v -> FlatIntMap v
{-# INLINABLE delete #-}
delete k m@(FlatIntMap vec) =
    case binarySearch vec k of
        Left _  -> m
        Right i -> FlatIntMap (V.unsafeDeleteIndex vec i)

-- | /O(N)/ Modify a value by key.
--
-- The value is evaluated to WHNF before writing into map.
adjust' :: (v -> v) -> Int -> FlatIntMap v -> FlatIntMap v
{-# INLINABLE adjust' #-}
adjust' f k m@(FlatIntMap vec) =
    case binarySearch vec k of
        Left _  -> m
        Right i -> FlatIntMap . V.unsafeModifyIndex vec i $
            \ (V.IPair k' v) -> let !v' = f v in V.IPair k' v'

-- | /O(n+m)/ Merge two 'FlatIntMap', prefer right value on key duplication.
merge :: forall v. FlatIntMap v -> FlatIntMap v -> FlatIntMap v
{-# INLINABLE merge #-}
merge fmL@(FlatIntMap (V.Vector arrL sL lL)) fmR@(FlatIntMap (V.Vector arrR sR lR))
    | null fmL = fmR
    | null fmR = fmL
    | otherwise = FlatIntMap (V.createN (lL+lR) (go sL sR 0))
  where
    endL = sL + lL
    endR = sR + lR
    go :: Int -> Int -> Int -> A.SmallMutableArray s (V.IPair v) -> ST s Int
    go !i !j !k marr
        | i >= endL = do
            A.copySmallArray marr k arrR j (lR-j)
            return $! k+lR-j
        | j >= endR = do
            A.copySmallArray marr k arrL i (lL-i)
            return $! k+lL-i
        | otherwise = do
            kvL@(V.IPair kL _) <- arrL `A.indexSmallArrayM` i
            kvR@(V.IPair kR _) <- arrR `A.indexSmallArrayM` j
            case kL `compare` kR of LT -> do A.writeSmallArray marr k kvL
                                             go (i+1) j (k+1) marr
                                    EQ -> do A.writeSmallArray marr k kvR
                                             go (i+1) (j+1) (k+1) marr
                                    _  -> do A.writeSmallArray marr k kvR
                                             go i (j+1) (k+1) marr

-- | /O(n+m)/ Merge two 'FlatIntMap' with a merge function.
mergeWithKey' :: forall v. (Int -> v -> v -> v) -> FlatIntMap v -> FlatIntMap v -> FlatIntMap v
{-# INLINABLE mergeWithKey' #-}
mergeWithKey' f fmL@(FlatIntMap (V.Vector arrL sL lL)) fmR@(FlatIntMap (V.Vector arrR sR lR))
    | null fmL = fmR
    | null fmR = fmL
    | otherwise = FlatIntMap (V.createN (lL+lR) (go sL sR 0))
  where
    endL = sL + lL
    endR = sR + lR
    go :: Int -> Int -> Int -> A.SmallMutableArray s (V.IPair v) -> ST s Int
    go !i !j !k marr
        | i >= endL = do
            A.copySmallArray marr k arrR j (lR-j)
            return $! k+lR-j
        | j >= endR = do
            A.copySmallArray marr k arrL i (lL-i)
            return $! k+lL-i
        | otherwise = do
            kvL@(V.IPair kL vL) <- arrL `A.indexSmallArrayM` i
            kvR@(V.IPair kR vR) <- arrR `A.indexSmallArrayM` j
            case kL `compare` kR of LT -> do A.writeSmallArray marr k kvL
                                             go (i+1) j (k+1) marr
                                    EQ -> do let !v' = f kL vL vR
                                             A.writeSmallArray marr k (V.IPair kL v')
                                             go (i+1) (j+1) (k+1) marr
                                    _  -> do A.writeSmallArray marr k kvR
                                             go i (j+1) (k+1) marr

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
--
-- During folding k is in descending order.
foldrWithKey :: (Int -> v -> a -> a) -> a -> FlatIntMap v -> a
{-# INLINE foldrWithKey #-}
foldrWithKey f a (FlatIntMap vs) = foldr (\ (V.IPair k v) a' -> f k v a') a vs

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
--
-- During folding Int is in ascending order.
foldlWithKey :: (a -> Int -> v -> a) -> a -> FlatIntMap v -> a
{-# INLINE foldlWithKey #-}
foldlWithKey f a (FlatIntMap vs) = foldl (\ a' (V.IPair k v) -> f a' k v) a vs

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
--
-- During folding Int is in descending order.
foldrWithKey' :: (Int -> v -> a -> a) -> a -> FlatIntMap v -> a
{-# INLINE foldrWithKey' #-}
foldrWithKey' f a (FlatIntMap vs) = V.foldr' (\ (V.IPair k v) -> f k v) a vs

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
--
-- During folding Int is in ascending order.
foldlWithKey' :: (a -> Int -> v -> a) -> a -> FlatIntMap v -> a
{-# INLINE foldlWithKey' #-}
foldlWithKey' f a (FlatIntMap vs) = V.foldl' (\ a' (V.IPair k v) -> f a' k v) a vs

-- | /O(n)/.
--
-- @'traverseWithKey' f s == 'pack' \<$\> 'traverse' (\(k, v) -> (,) k \<$\> f k v) ('unpack' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
traverseWithKey :: Applicative t => (Int -> a -> t b) -> FlatIntMap a -> t (FlatIntMap b)
{-# INLINE traverseWithKey #-}
traverseWithKey f (FlatIntMap vs) = FlatIntMap <$> V.traverse (\ (V.IPair k v) -> V.IPair k <$> f k v) vs

--------------------------------------------------------------------------------

-- | Find the key's index in the vector slice, if key exists return 'Right',
-- otherwise 'Left', i.e. the insert index
--
-- This function only works on ascending sorted vectors.
binarySearch :: V.Vector (V.IPair v) -> Int -> Either Int Int
{-# INLINABLE binarySearch #-}
binarySearch (V.Vector _ _ 0) _   = Left 0
binarySearch (V.Vector arr s0 l) !k' = go s0 (s0+l-1)
  where
    go !s !e
        | s == e =
            let V.IPair k _  = arr `A.indexSmallArray` s
            in case k' `compare` k of LT -> Left s
                                      GT -> let !s' = s+1 in Left s'
                                      _  -> Right s
        | s >  e = Left s
        | otherwise =
            let !mid = (s+e) `unsafeShiftR` 1
                (V.IPair k _)  = arr `A.indexSmallArray` mid
            in case k' `compare` k of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Right mid
