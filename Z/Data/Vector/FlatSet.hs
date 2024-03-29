{-|
Module      : Z.Data.Vector.FlatSet
Description : Fast set based on sorted vector
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides a simple value set based on sorted vector and binary search. It's particularly
suitable for small sized value collections such as deserializing intermediate representation.
But can also used in various place where insertion and deletion is rare but require fast elem.

-}

module Z.Data.Vector.FlatSet
  ( -- * FlatSet backed by sorted vector
    FlatSet, sortedValues, size, null, empty, map'
  , pack, packN, packR, packRN
  , unpack, unpackR, packVector, packVectorR
  , elem
  , delete
  , insert
  , merge
    -- * search on vectors
  , binarySearch
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Primitive.SmallArray  as A
import qualified Data.Semigroup             as Semigroup
import qualified Data.Monoid                as Monoid
import qualified Z.Data.Vector.Base         as V
import qualified Z.Data.Vector.Extra        as V
import qualified Z.Data.Vector.Sort         as V
import qualified Z.Data.Text.Print          as T
import           Data.Bits                   (unsafeShiftR)
import           Data.Data
import           Prelude hiding (elem, null)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

--------------------------------------------------------------------------------

newtype FlatSet v = FlatSet { sortedValues :: V.Vector v }
    deriving (Show, Eq, Ord, Typeable, Foldable)

instance T.Print v => T.Print (FlatSet v) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP p (FlatSet vec) = T.parenWhen (p > 10) $ do
        "FlatSet{"
        T.intercalateVec T.comma T.toUTF8Builder vec
        T.char7 '}'

instance Ord v => Semigroup.Semigroup (FlatSet v) where
    {-# INLINE (<>) #-}
    (<>) = merge

instance Ord v => Monoid.Monoid (FlatSet v) where
    {-# INLINE mappend #-}
    mappend = (<>)
    {-# INLINE mempty #-}
    mempty = empty

instance NFData v => NFData (FlatSet v) where
    {-# INLINE rnf #-}
    rnf (FlatSet vs) = rnf vs

instance (Ord v, Arbitrary v) => Arbitrary (FlatSet v) where
    arbitrary = pack <$> arbitrary
    shrink v = pack <$> shrink (unpack v)

instance (CoArbitrary v) => CoArbitrary (FlatSet v) where
    coarbitrary = coarbitrary . unpack

size :: FlatSet v -> Int
{-# INLINE size #-}
size = V.length . sortedValues

null :: FlatSet v -> Bool
{-# INLINE null #-}
null = V.null . sortedValues

-- | Mapping values of within a set, the result size may change if there're duplicated values
-- after mapping.
map' :: forall v. Ord v => (v -> v) -> FlatSet v -> FlatSet v
{-# INLINE map' #-}
map' f (FlatSet vs) = packVector (V.map' f vs :: V.Vector v)

-- | /O(1)/ empty flat set.
empty :: FlatSet v
{-# NOINLINE empty #-}
empty = FlatSet V.empty

-- | /O(N*logN)/ Pack list of values, on duplication prefer left one.
pack :: Ord v => [v] -> FlatSet v
{-# INLINABLE pack #-}
pack vs = FlatSet (V.mergeDupAdjacentLeft (==) (V.mergeSort (V.pack vs)))

-- | /O(N*logN)/ Pack list of values with suggested size, on duplication prefer left one.
packN :: Ord v => Int -> [v] -> FlatSet v
{-# INLINABLE packN #-}
packN n vs = FlatSet (V.mergeDupAdjacentLeft (==) (V.mergeSort (V.packN n vs)))

-- | /O(N*logN)/ Pack list of values, on duplication prefer right one.
packR :: Ord v => [v] -> FlatSet v
{-# INLINABLE packR #-}
packR vs = FlatSet (V.mergeDupAdjacentRight (==) (V.mergeSort (V.pack vs)))

-- | /O(N*logN)/ Pack list of values with suggested size, on duplication prefer right one.
packRN :: Ord v => Int -> [v] -> FlatSet v
{-# INLINABLE packRN #-}
packRN n vs = FlatSet (V.mergeDupAdjacentRight (==) (V.mergeSort (V.packN n vs)))

-- | /O(N)/ Unpack a set of values to a list s in ascending order.
--
-- This function works with @foldr/build@ fusion in base.
unpack :: FlatSet v -> [v]
{-# INLINE unpack #-}
unpack = V.unpack . sortedValues

-- | /O(N)/ Unpack a set of values to a list s in descending order.
--
-- This function works with @foldr/build@ fusion in base.
unpackR :: FlatSet v -> [v]
{-# INLINE unpackR #-}
unpackR = V.unpackR . sortedValues

-- | /O(N*logN)/ Pack vector of values, on duplication prefer left one.
packVector :: Ord v => V.Vector v -> FlatSet v
{-# INLINABLE packVector #-}
packVector vs = FlatSet (V.mergeDupAdjacentLeft (==) (V.mergeSort vs))

-- | /O(N*logN)/ Pack vector of values, on duplication prefer right one.
packVectorR :: Ord v => V.Vector v -> FlatSet v
{-# INLINABLE packVectorR #-}
packVectorR vs = FlatSet (V.mergeDupAdjacentRight (==) (V.mergeSort vs))

-- | /O(logN)/ Binary search on flat set.
elem :: Ord v => v -> FlatSet v -> Bool
{-# INLINABLE elem #-}
elem v (FlatSet vec) = case binarySearch vec v of Left _ -> False
                                                  _      -> True
-- | /O(N)/ Insert new value into set.
insert :: Ord v => v -> FlatSet v -> FlatSet v
{-# INLINABLE insert #-}
insert v m@(FlatSet vec) =
    case binarySearch vec v of
        Left i -> FlatSet (V.unsafeInsertIndex vec i v)
        Right _ -> m

-- | /O(N)/ Delete a value from set.
delete :: Ord v => v -> FlatSet v -> FlatSet v
{-# INLINABLE delete #-}
delete v m@(FlatSet vec) =
    case binarySearch vec v of
        Left _ -> m
        Right i -> FlatSet (V.unsafeDeleteIndex vec i)

-- | /O(n+m)/ Merge two 'FlatSet', prefer right value on value duplication.
merge :: forall v . Ord v => FlatSet v -> FlatSet v -> FlatSet v
{-# INLINABLE merge #-}
merge fmL@(FlatSet (V.Vector arrL sL lL)) fmR@(FlatSet (V.Vector arrR sR lR))
    | null fmL = fmR
    | null fmR = fmL
    | otherwise = FlatSet (V.createN (lL+lR) (go sL sR 0))
  where
    endL = sL + lL
    endR = sR + lR
    go :: Int -> Int -> Int -> A.SmallMutableArray s v -> ST s Int
    go !i !j !k marr
        | i >= endL = do
            A.copySmallArray marr k arrR j (lR-j)
            return $! k+lR-j
        | j >= endR = do
            A.copySmallArray marr k arrL i (lL-i)
            return $! k+lL-i
        | otherwise = do
            vL <- arrL `A.indexSmallArrayM` i
            vR <- arrR `A.indexSmallArrayM` j
            case vL `compare` vR of LT -> do A.writeSmallArray marr k vL
                                             go (i+1) j (k+1) marr
                                    EQ -> do A.writeSmallArray marr k vR
                                             go (i+1) (j+1) (k+1) marr
                                    _  -> do A.writeSmallArray marr k vR
                                             go i (j+1) (k+1) marr

--------------------------------------------------------------------------------

-- | Find the value's index in the vector, if value exists return 'Right',
-- otherwise 'Left', i.e. the insert index
--
-- This function only works on ascending sorted vectors.
binarySearch :: Ord v => V.Vector v -> v -> Either Int Int
{-# INLINABLE binarySearch #-}
binarySearch (V.Vector _ _ 0) _   = Left 0
binarySearch (V.Vector arr s0 l) !v' = go s0 (s0+l-1)
  where
    go !s !e
        | s == e =
            let v = arr `A.indexSmallArray` s
            in case v' `compare` v of LT -> Left (s-s0)
                                      GT -> let !s' = s+1 in Left (s'-s0)
                                      _  -> Right (s-s0)
        | s >  e = Left s
        | otherwise =
            let !mid = (s+e) `unsafeShiftR` 1
                v = arr `A.indexSmallArray` mid
            in case v' `compare` v of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Right (mid-s0)
