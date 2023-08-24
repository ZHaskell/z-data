{-|
Module      : Z.Data.FlatContainer.FlatPrimSet
Description : Fast int set based on sorted vector
Copyright   : (c) Dong Han, 2017-2019
              (c) Tao He, 2018-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides a simple int set based on sorted vector and binary search. It's particularly
suitable for small sized value collections such as deserializing intermediate representation.
But can also used in various place where insertion and deletion is rare but require fast elem.

-}

module Z.Data.FlatContainer.FlatPrimSet
  ( -- * FlatIntSet backed by sorted vector
    FlatIntSet, sortedValues, size, null, empty, map'
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
import qualified Data.Semigroup             as Semigroup
import qualified Data.Monoid                as Monoid
import qualified Data.Primitive.PrimArray   as A
import qualified Z.Data.Vector.Base         as V
import qualified Z.Data.Vector.Extra        as V
import qualified Z.Data.Vector.Sort         as V
import qualified Z.Data.Text.Print          as T
import           Data.Bits                   (unsafeShiftR)
import           Data.Data
import           Prelude hiding (elem, null)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))

--------------------------------------------------------------------------------

newtype FlatPrimSet = FlatPrimSet { sortedValues :: V.PrimVector a }
    deriving (Show, Eq, Ord, Typeable)

instance T.Print FlatIntSet where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP p (FlatIntSet vec) = T.parenWhen (p > 10) $ do
        "FlatIntSet{"
        T.intercalateVec T.comma T.toUTF8Builder vec
        T.char7 '}'

instance Semigroup.Semigroup FlatIntSet where
    {-# INLINE (<>) #-}
    (<>) = merge

instance Monoid.Monoid FlatIntSet where
    {-# INLINE mappend #-}
    mappend = (<>)
    {-# INLINE mempty #-}
    mempty = empty

instance NFData FlatIntSet where
    {-# INLINE rnf #-}
    rnf (FlatIntSet vs) = rnf vs

instance Arbitrary FlatIntSet where
    arbitrary = pack <$> arbitrary
    shrink v = pack <$> shrink (unpack v)

instance CoArbitrary FlatIntSet where
    coarbitrary = coarbitrary . unpack

size :: FlatIntSet -> Int
{-# INLINE size #-}
size = V.length . sortedValues

null :: FlatIntSet -> Bool
{-# INLINE null #-}
null = V.null . sortedValues

-- | Mapping values of within a set, the result size may change if there're duplicated values
-- after mapping.
map' :: (Int -> Int) -> FlatIntSet -> FlatIntSet
{-# INLINE map' #-}
map' f (FlatIntSet vs) = packVector (V.map' f vs)

-- | /O(1)/ empty flat set.
empty :: FlatIntSet
{-# NOINLINE empty #-}
empty = FlatIntSet V.empty

-- | /O(N*logN)/ Pack list of values, on duplication prefer left one.
pack :: [Int] -> FlatIntSet
{-# INLINABLE pack #-}
pack vs = FlatIntSet (V.mergeDupAdjacentLeft (==) (V.mergeSort (V.pack vs)))

-- | /O(N*logN)/ Pack list of values with suggested size, on duplication prefer left one.
packN :: Int -> [Int] -> FlatIntSet
{-# INLINABLE packN #-}
packN n vs = FlatIntSet (V.mergeDupAdjacentLeft (==) (V.mergeSort (V.packN n vs)))

-- | /O(N*logN)/ Pack list of values, on duplication prefer right one.
packR :: [Int] -> FlatIntSet
{-# INLINABLE packR #-}
packR vs = FlatIntSet (V.mergeDupAdjacentRight (==) (V.mergeSort (V.pack vs)))

-- | /O(N*logN)/ Pack list of values with suggested size, on duplication prefer right one.
packRN :: Int -> [Int] -> FlatIntSet
{-# INLINABLE packRN #-}
packRN n vs = FlatIntSet (V.mergeDupAdjacentRight (==) (V.mergeSort (V.packN n vs)))

-- | /O(N)/ Unpack a set of values to a list s in ascending order.
--
-- This function works with @foldr/build@ fusion in base.
unpack :: FlatIntSet -> [Int]
{-# INLINE unpack #-}
unpack = V.unpack . sortedValues

-- | /O(N)/ Unpack a set of values to a list s in descending order.
--
-- This function works with @foldr/build@ fusion in base.
unpackR :: FlatIntSet -> [Int]
{-# INLINE unpackR #-}
unpackR = V.unpackR . sortedValues

-- | /O(N*logN)/ Pack vector of values, on duplication prefer left one.
packVector :: V.PrimVector Int -> FlatIntSet
{-# INLINABLE packVector #-}
packVector vs = FlatIntSet (V.mergeDupAdjacentLeft (==) (V.mergeSort vs))

-- | /O(N*logN)/ Pack vector of values, on duplication prefer right one.
packVectorR :: V.PrimVector Int -> FlatIntSet
{-# INLINABLE packVectorR #-}
packVectorR vs = FlatIntSet (V.mergeDupAdjacentRight (==) (V.mergeSort vs))

-- | /O(logN)/ Binary search on flat set.
elem :: Int -> FlatIntSet -> Bool
{-# INLINABLE elem #-}
elem v (FlatIntSet vec) = case binarySearch vec v of Left _ -> False
                                                     _      -> True

-- | /O(N)/ Insert new value into set.
insert :: Int -> FlatIntSet -> FlatIntSet
{-# INLINABLE insert #-}
insert v m@(FlatIntSet vec) =
    case binarySearch vec v of
        Left i -> FlatIntSet (V.unsafeInsertIndex vec i v)
        Right _ -> m

-- | /O(N)/ Delete a value.
delete :: Int -> FlatIntSet -> FlatIntSet
{-# INLINABLE delete #-}
delete v m@(FlatIntSet vec) =
    case binarySearch vec v of
        Left _ -> m
        Right i -> FlatIntSet (V.unsafeDeleteIndex vec i)

-- | /O(n+m)/ Merge two 'FlatIntSet', prefer right value on value duplication.
merge :: FlatIntSet -> FlatIntSet -> FlatIntSet
{-# INLINABLE merge #-}
merge fmL@(FlatIntSet (V.PrimVector arrL sL lL)) fmR@(FlatIntSet (V.PrimVector arrR sR lR))
    | null fmL = fmR
    | null fmR = fmL
    | otherwise = FlatIntSet (V.createN (lL+lR) (go sL sR 0))
  where
    endL = sL + lL
    endR = sR + lR
    go :: Int -> Int -> Int -> A.MutablePrimArray s Int -> ST s Int
    go !i !j !k marr
        | i >= endL = do
            A.copyPrimArray marr k arrR j (lR-j)
            return $! k+lR-j
        | j >= endR = do
            A.copyPrimArray marr k arrL i (lL-i)
            return $! k+lL-i
        | otherwise = do
            let !vL = arrL `A.indexPrimArray` i
            let !vR = arrR `A.indexPrimArray` j
            case vL `compare` vR of LT -> do A.writePrimArray marr k vL
                                             go (i+1) j (k+1) marr
                                    EQ -> do A.writePrimArray marr k vR
                                             go (i+1) (j+1) (k+1) marr
                                    _  -> do A.writePrimArray marr k vR
                                             go i (j+1) (k+1) marr

--------------------------------------------------------------------------------

-- | Find the value's index in the vector slice, if value exists return 'Right',
-- otherwise 'Left', i.e. the insert index
--
-- This function only works on ascending sorted vectors.
binarySearch :: V.PrimVector Int -> Int -> Either Int Int
{-# INLINABLE binarySearch #-}
binarySearch (V.PrimVector _ _ 0) _   = Left 0
binarySearch (V.PrimVector arr s0 l) !v' = go s0 (s0+l-1)
  where
    go !s !e
        | s == e =
            let v = arr `A.indexPrimArray` s
            in case v' `compare` v of LT -> Left s
                                      GT -> let !s' = s+1 in Left s'
                                      _  -> Right s
        | s >  e = Left s
        | otherwise =
            let !mid = (s+e) `unsafeShiftR` 1
                v = arr `A.indexPrimArray` mid
            in case v' `compare` v of LT -> go s (mid-1)
                                      GT -> go (mid+1) e
                                      _  -> Right mid
