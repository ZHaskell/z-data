{-|
Module      : Z.Data.Array.Checked
Description : Bounded checked boxed and unboxed arrays
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides exactly the same API with "Z.Data.Array", but will throw an 'IndexOutOfBounds'
'ArrayException' on bound check failure, it's useful when debugging array algorithms: just swap this
module with "Z.Data.Array", segmentation faults caused by out bound access will be turned into exceptions
with more informations.

-}
module Z.Data.Array.Checked
  ( -- * Arr typeclass re-export
    Arr, MArr
  , A.singletonArr, A.doubletonArr
  , modifyIndexArr, insertIndexArr, deleteIndexArr
  , RealWorld
  -- * Boxed array type
  , A.Array(..)
  , A.MutableArray(..)
  , A.SmallArray(..)
  , A.SmallMutableArray(..)
  , A.uninitialized
  -- * Primitive array type
  , A.PrimArray(..)
  , A.MutablePrimArray(..)
  , Prim(..)
  -- * Bound checked array operations
  , newArr
  , newArrWith
  , readArr
  , writeArr
  , setArr
  , indexArr
  , indexArr'
  , indexArrM
  , freezeArr
  , thawArr
  , copyArr
  , copyMutableArr
  , moveArr
  , cloneArr
  , cloneMutableArr
  , resizeMutableArr
  , shrinkMutableArr
  -- * No bound checked operations
  , A.unsafeFreezeArr
  , A.unsafeThawArr
  , A.sameMutableArr
  , A.sizeofArr
  , A.sizeofMutableArr
  , A.sameArr
  -- * Bound checked primitive array operations
  , newPinnedPrimArray, newAlignedPinnedPrimArray
  , copyPrimArrayToPtr, copyMutablePrimArrayToPtr, copyPtrToMutablePrimArray
  -- * No bound checked primitive array operations
  , A.primArrayContents, A.mutablePrimArrayContents, A.withPrimArrayContents, A.withMutablePrimArrayContents
  , A.isPrimArrayPinned, A.isMutablePrimArrayPinned
  -- * Unlifted array type
  , A.UnliftedArray(..)
  , A.MutableUnliftedArray(..)
  , A.PrimUnlifted(..)
  -- * The 'ArrayException' type
  , ArrayException(..)
  -- * Cast between primitive arrays
  , A.Cast
  , A.castArray
  , A.castMutableArray
  -- * Re-export
  , sizeOf
  ) where

import           Control.Exception       (ArrayException (..), throw)
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Primitive.Types
import           GHC.Stack
import           Z.Data.Array          (Arr, MArr)
import qualified Z.Data.Array          as A

check :: HasCallStack => Bool -> a -> a
{-# INLINE check #-}
check True  x = x
check False _ = throw (IndexOutOfBounds $ show callStack)

newArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
       => Int -> m (MArr arr s a)
newArr n = check  (n>=0) (A.newArr n)
{-# INLINE newArr #-}

newArrWith :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
           => Int -> a -> m (MArr arr s a)
newArrWith n x = check  (n>=0) (A.newArrWith n x)
{-# INLINE newArrWith #-}

readArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => MArr arr s a -> Int -> m a
readArr marr i = do
    siz <- A.sizeofMutableArr marr
    check
        (i>=0 && i<siz)
        (A.readArr marr i)
{-# INLINE readArr #-}

writeArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
         => MArr arr s a -> Int -> a -> m ()
writeArr marr i x = do
    siz <- A.sizeofMutableArr marr
    check
        (i>=0 && i<siz)
        (A.writeArr marr i x)
{-# INLINE writeArr #-}

setArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
       => MArr arr s a -> Int -> Int -> a -> m ()
setArr marr s l x = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.setArr marr s l x)
{-# INLINE setArr #-}

indexArr :: (Arr arr a, HasCallStack)
         => arr a -> Int -> a
indexArr arr i = check
    (i>=0 && i<A.sizeofArr arr)
    (A.indexArr arr i)
{-# INLINE indexArr #-}

indexArr' :: (Arr arr a, HasCallStack)
          => arr a -> Int -> (# a #)
indexArr' arr i =
    if (i>=0 && i<A.sizeofArr arr)
    then A.indexArr' arr i
    else throw (IndexOutOfBounds $ show callStack)
{-# INLINE indexArr' #-}

indexArrM :: (Arr arr a, Monad m, HasCallStack)
          => arr a -> Int -> m a
indexArrM arr i = check
    (i>=0 && i<A.sizeofArr arr)
    (A.indexArrM arr i)
{-# INLINE indexArrM #-}

freezeArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
          => MArr arr s a -> Int -> Int -> m (arr a)
freezeArr marr s l = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.freezeArr marr s l)
{-# INLINE freezeArr #-}

thawArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => arr a -> Int -> Int -> m (MArr arr s a)
thawArr arr s l = check
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.thawArr arr s l)
{-# INLINE thawArr #-}

copyArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => MArr arr s a -> Int -> arr a -> Int -> Int -> m ()
copyArr marr s1 arr s2 l = do
    siz <- A.sizeofMutableArr marr
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofArr arr && (s1+l)<=siz)
        (A.copyArr marr s1 arr s2 l)
{-# INLINE copyArr #-}

copyMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
               => MArr arr s a -> Int -> MArr arr s a -> Int -> Int -> m ()
copyMutableArr marr1 s1 marr2 s2 l = do
    siz1 <- A.sizeofMutableArr marr1
    siz2 <- A.sizeofMutableArr marr2
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
        (A.copyMutableArr marr1 s1 marr2 s2 l)
{-# INLINE copyMutableArr #-}

moveArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => MArr arr s a -> Int -> MArr arr s a -> Int -> Int -> m ()
moveArr marr1 s1 marr2 s2 l = do
    siz1 <- A.sizeofMutableArr marr1
    siz2 <- A.sizeofMutableArr marr2
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
        (A.copyMutableArr marr1 s1 marr2 s2 l)
{-# INLINE moveArr #-}

cloneArr :: (Arr arr a, HasCallStack)
         => arr a -> Int -> Int -> arr a
cloneArr arr s l = check
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.cloneArr arr s l)
{-# INLINE cloneArr #-}

cloneMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                => MArr arr s a -> Int -> Int -> m (MArr arr s a)
cloneMutableArr marr s l = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.cloneMutableArr marr s l)
{-# INLINE cloneMutableArr #-}

resizeMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                 => MArr arr s a -> Int -> m (MArr arr s a)
resizeMutableArr marr n = check
    (n>=0)
    (A.resizeMutableArr marr n)
{-# INLINE resizeMutableArr #-}

-- | New size should be >= 0, and <= original size.
--
shrinkMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                 => MArr arr s a -> Int -> m ()
shrinkMutableArr marr n = do
    siz <- A.sizeofMutableArr marr
    check
        (n>=0 && n<=siz)
        (A.shrinkMutableArr marr n)
{-# INLINE shrinkMutableArr #-}

--------------------------------------------------------------------------------

-- | Create a /pinned/ byte array of the specified size,
-- The garbage collector is guaranteed not to move it.
newPinnedPrimArray :: (PrimMonad m, Prim a, HasCallStack)
                   => Int -> m (A.MutablePrimArray (PrimState m) a)
{-# INLINE newPinnedPrimArray #-}
newPinnedPrimArray n =
    check  (n>=0) (A.newPinnedPrimArray n)

-- | Create a /pinned/ primitive array of the specified size and respect given primitive type's
-- alignment. The garbage collector is guaranteed not to move it.
--
newAlignedPinnedPrimArray :: (PrimMonad m, Prim a, HasCallStack)
                          => Int -> m (A.MutablePrimArray (PrimState m) a)
{-# INLINE newAlignedPinnedPrimArray #-}
newAlignedPinnedPrimArray n =
    check  (n>=0) (A.newAlignedPinnedPrimArray n)

copyPrimArrayToPtr :: (PrimMonad m, Prim a, HasCallStack)
                   => Ptr a
                   -> A.PrimArray a
                   -> Int
                   -> Int
                   -> m ()
{-# INLINE copyPrimArrayToPtr #-}
copyPrimArrayToPtr ptr arr s l = check
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.copyPrimArrayToPtr ptr arr s l)

copyMutablePrimArrayToPtr :: (PrimMonad m, Prim a, HasCallStack)
                          => Ptr a
                          -> A.MutablePrimArray (PrimState m) a
                          -> Int
                          -> Int
                          -> m ()
{-# INLINE copyMutablePrimArrayToPtr #-}
copyMutablePrimArrayToPtr ptr marr s l = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.copyMutablePrimArrayToPtr ptr marr s l)

copyPtrToMutablePrimArray :: (PrimMonad m, Prim a, HasCallStack)
                            => A.MutablePrimArray (PrimState m) a
                            -> Int
                            -> Ptr a
                            -> Int
                            -> m ()
{-# INLINE copyPtrToMutablePrimArray #-}
copyPtrToMutablePrimArray marr s ptr l = do
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.copyPtrToMutablePrimArray marr s ptr l)

--------------------------------------------------------------------------------

modifyIndexArr :: (Arr arr a, HasCallStack) => arr a
               -> Int    -- ^ offset
               -> Int    -- ^ length
               -> Int    -- ^ index in new array
               -> (a -> a)   -- ^ modify function
               -> arr a
{-# INLINE modifyIndexArr #-}
modifyIndexArr arr off len ix f = runST $ do
    marr <- A.unsafeThawArr (cloneArr arr off len)
    !v <- f <$> readArr marr ix
    writeArr marr ix v
    A.unsafeFreezeArr marr

-- | Insert an immutable array's element at given index to produce a new array.
insertIndexArr :: Arr arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ insert index in new array
               -> a          -- ^ element to be inserted
               -> arr a
{-# INLINE insertIndexArr #-}
insertIndexArr arr s l i x = runST $ do
    marr <- newArrWith (l+1) x
    when (i>0) $ copyArr marr 0 arr s i
    when (i<l) $ copyArr marr (i+1) arr (i+s) (l-i)
    A.unsafeFreezeArr marr

-- | Drop an immutable array's element at given index to produce a new array.
deleteIndexArr :: Arr arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ drop index in new array
               -> arr a
{-# INLINE deleteIndexArr #-}
deleteIndexArr arr s l i = runST $ do
    marr <- newArr (l-1)
    when (i>0) $ copyArr marr 0 arr s i
    let i' = i+1
    when (i'<l) $ copyArr marr i arr (i'+s) (l-i')
    A.unsafeFreezeArr marr
