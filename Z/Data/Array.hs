{-|
Module      : Z.Data.Array.Checked
Description : Bounded checked boxed and unboxed arrays
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Unified unboxed and boxed array operations using type family. This module re-export "Z.Data.Array.Base" module, but add check
when @check-array-bound@ flag is set. To debug array algorithms just add @Z-Data: -f+check-array-bound@ to your local @cabal.project@ file.
otherwise, none of the operations are bound checked.

Some mnemonics:

  * 'newArr' and 'newArrWith' return mutable array.
    'readArr' and 'writeArr' perform read and write actions on mutable arrays.
    'setArr' fills the elements with offset and length.
    'indexArr' only works on immutable Array, use 'indexArr'' to avoid thunks building up in the heap.

  * 'freezeArr' and 'thawArr' make a copy thus need slicing params.
    'unsafeFreezeArr' and 'unsafeThawArr' DO NOT COPY, use with care.

  * The order of arguements of 'copyArr', 'copyMutableArr' and 'moveArr' are always target and its offset
    come first, and source and source offset follow, copying length comes last.

-}
module Z.Data.Array
  ( -- * Arr typeclass re-export
    Arr, MArr
  , A.emptyArr, A.singletonArr, A.doubletonArr
  , modifyIndexArr, insertIndexArr, deleteIndexArr, swapArr, swapMutableArr
  , A.doubleMutableArr, shuffleMutableArr
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

import           Control.Exception              (ArrayException (..), throw)
import           Control.Monad.Primitive
import           Data.Primitive.Types
import           GHC.Stack
import           System.Random.Stateful         (StatefulGen)  
import           Z.Data.Array.Base              (Arr, MArr)
import qualified Z.Data.Array.Base              as A
import           Control.Monad.ST
#ifdef CHECK_ARRAY_BOUND
import           Control.Monad
#endif

#ifdef CHECK_ARRAY_BOUND
check :: HasCallStack => Bool -> a -> a
{-# INLINE check #-}
check True  x = x
check False _ = throw (IndexOutOfBounds $ show callStack)
#endif

-- | Make a new array with a given size.
--
-- For boxed arrays, all elements are 'uninitialized' , which shall not be accessed.
-- For primitive arrays, elements are just random garbage.
newArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
       => Int -> m (MArr arr s a)
newArr n =
#ifdef CHECK_ARRAY_BOUND
    check  (n>=0) (A.newArr n)
#else
    A.newArr n
#endif
{-# INLINE newArr #-}

-- | Make a new array and fill it with an initial value.
newArrWith :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
           => Int -> a -> m (MArr arr s a)
newArrWith n x =
#ifdef CHECK_ARRAY_BOUND
    check  (n>=0) (A.newArrWith n x)
#else
    (A.newArrWith n x)
#endif
{-# INLINE newArrWith #-}

-- | Read from specified index of mutable array in a primitive monad.
readArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => MArr arr s a -> Int -> m a
readArr marr i = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (i>=0 && i<siz)
        (A.readArr marr i)
#else
        (A.readArr marr i)
#endif
{-# INLINE readArr #-}

-- | Write to specified index of mutable array in a primitive monad.
writeArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
         => MArr arr s a -> Int -> a -> m ()
writeArr marr i x = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (i>=0 && i<siz)
        (A.writeArr marr i x)
#else
        (A.writeArr marr i x)
#endif
{-# INLINE writeArr #-}

-- | Fill the mutable array with a given value.
setArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
       => MArr arr s a -> Int -> Int -> a -> m ()
setArr marr s l x = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.setArr marr s l x)
#else
        (A.setArr marr s l x)
#endif
{-# INLINE setArr #-}

-- | Read from the specified index of an immutable array. It's pure and often
-- results in an indexing thunk for lifted arrays, use 'indexArr\'' or 'indexArrM' to avoid this.
indexArr :: (Arr arr a, HasCallStack)
         => arr a -> Int -> a
indexArr arr i =
#ifdef CHECK_ARRAY_BOUND
    check (i>=0 && i<A.sizeofArr arr) (A.indexArr arr i)
#else
    (A.indexArr arr i)
#endif
{-# INLINE indexArr #-}

-- | Read from the specified index of an immutable array. The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen but does not evaluate the element itself.
-- Evaluating the thunk prevents additional thunks from building up on the heap.
-- Avoiding these thunks, in turn, reduces references to the argument array, allowing it to be garbage collected more promptly.
indexArr' :: (Arr arr a, HasCallStack)
          => arr a -> Int -> (# a #)
indexArr' arr i =
#ifdef CHECK_ARRAY_BOUND
    if (i>=0 && i<A.sizeofArr arr)
    then A.indexArr' arr i
    else throw (IndexOutOfBounds $ show callStack)
#else
    (A.indexArr' arr i)
#endif
{-# INLINE indexArr' #-}

-- | Monadically read a value from the immutable array at the given index.
-- This allows us to be strict in the array while remaining lazy in the read
-- element which is very useful for collective operations. Suppose we want to
-- copy an array. We could do something like this:
--
-- > copy marr arr ... = do ...
-- >                        writeArray marr i (indexArray arr i) ...
-- >                        ...
--
-- But since primitive arrays are lazy, the calls to 'indexArray' will not be
-- evaluated. Rather, @marr@ will be filled with thunks each of which would
-- retain a reference to @arr@. This is definitely not what we want!
--
-- With 'indexArrayM', we can instead write
--
-- > copy marr arr ... = do ...
-- >                        x <- indexArrayM arr i
-- >                        writeArray marr i x
-- >                        ...
--
-- Now, indexing is executed immediately although the returned element is
-- still not evaluated.
--
-- /Note:/ this function does not do bounds checking.
indexArrM :: (Arr arr a, Monad m, HasCallStack)
          => arr a -> Int -> m a
indexArrM arr i =
#ifdef CHECK_ARRAY_BOUND
    check
        (i>=0 && i<A.sizeofArr arr)
        (A.indexArrM arr i)
#else
        (A.indexArrM arr i)
#endif
{-# INLINE indexArrM #-}

-- | Create an immutable copy of a slice of an array.
-- This operation makes a copy of the specified section, so it is safe to continue using the mutable array afterward.
freezeArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
          => MArr arr s a -> Int -> Int -> m (arr a)
freezeArr marr s l = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.freezeArr marr s l)
#else
        (A.freezeArr marr s l)
#endif
{-# INLINE freezeArr #-}

-- | Create a mutable array from a slice of an immutable array.
-- This operation makes a copy of the specified slice, so it is safe to use the immutable array afterward.
thawArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => arr a -> Int -> Int -> m (MArr arr s a)
thawArr arr s l =
#ifdef CHECK_ARRAY_BOUND
    check
        (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
        (A.thawArr arr s l)
#else
    (A.thawArr arr s l)
#endif
{-# INLINE thawArr #-}

-- | Copy a slice of an immutable array to a mutable array at given offset.
copyArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => MArr arr s a -> Int -> arr a -> Int -> Int -> m ()
copyArr marr s1 arr s2 l = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofArr arr && (s1+l)<=siz)
        (A.copyArr marr s1 arr s2 l)
#else
        (A.copyArr marr s1 arr s2 l)
#endif
{-# INLINE copyArr #-}

-- | Copy a slice of a mutable array to another mutable array at given offset.
-- The two mutable arrays must not be the same.
copyMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
               => MArr arr s a -> Int -> MArr arr s a -> Int -> Int -> m ()
copyMutableArr marr1 s1 marr2 s2 l = do
#ifdef CHECK_ARRAY_BOUND
    siz1 <- A.sizeofMutableArr marr1
    siz2 <- A.sizeofMutableArr marr2
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
        (A.copyMutableArr marr1 s1 marr2 s2 l)
#else
        (A.copyMutableArr marr1 s1 marr2 s2 l)
#endif
{-# INLINE copyMutableArr #-}

-- | Copy a slice of a mutable array to a mutable array at given offset.
-- The two mutable arrays can be the same.
moveArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
        => MArr arr s a -> Int -> MArr arr s a -> Int -> Int -> m ()
moveArr marr1 s1 marr2 s2 l = do
#ifdef CHECK_ARRAY_BOUND
    siz1 <- A.sizeofMutableArr marr1
    siz2 <- A.sizeofMutableArr marr2
    check
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
        (A.moveArr marr1 s1 marr2 s2 l)
#else
        (A.moveArr marr1 s1 marr2 s2 l)
#endif
{-# INLINE moveArr #-}

-- | Create an immutable copy with the given subrange of the original array.
cloneArr :: (Arr arr a, HasCallStack)
         => arr a -> Int -> Int -> arr a
cloneArr arr s l =
#ifdef CHECK_ARRAY_BOUND
    check
        (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
        (A.cloneArr arr s l)
#else
    (A.cloneArr arr s l)
#endif
{-# INLINE cloneArr #-}

-- | Create a mutable copy the given subrange of the original array.
cloneMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                => MArr arr s a -> Int -> Int -> m (MArr arr s a)
cloneMutableArr marr s l = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.cloneMutableArr marr s l)
#else
        (A.cloneMutableArr marr s l)
#endif
{-# INLINE cloneMutableArr #-}

-- | Resize a mutable array to the given size.
resizeMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                 => MArr arr s a -> Int -> m (MArr arr s a)
resizeMutableArr marr n =
#ifdef CHECK_ARRAY_BOUND
    check (n>=0) (A.resizeMutableArr marr n)
#else
    (A.resizeMutableArr marr n)
#endif
{-# INLINE resizeMutableArr #-}

-- | Shrink a mutable array to the given size. This operation only works on primitive arrays.
-- For some array types, this is a no-op, e.g. 'sizeOfMutableArr' will not change.
--
-- New size should be >= 0, and <= original size.
shrinkMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s, HasCallStack)
                 => MArr arr s a -> Int -> m ()
shrinkMutableArr marr n = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (n>=0 && n<=siz)
        (A.shrinkMutableArr marr n)
#else
        (A.shrinkMutableArr marr n)
#endif
{-# INLINE shrinkMutableArr #-}

--------------------------------------------------------------------------------

-- | Create a /pinned/ byte array of the specified size,
-- The garbage collector is guaranteed not to move it.
newPinnedPrimArray :: (PrimMonad m, Prim a, HasCallStack)
                   => Int -> m (A.MutablePrimArray (PrimState m) a)
{-# INLINE newPinnedPrimArray #-}
newPinnedPrimArray n =
#ifdef CHECK_ARRAY_BOUND
    check  (n>=0) (A.newPinnedPrimArray n)
#else
    (A.newPinnedPrimArray n)
#endif

-- | Create a /pinned/ primitive array of the specified size and respect given primitive type's
-- alignment. The garbage collector is guaranteed not to move it.
--
newAlignedPinnedPrimArray :: (PrimMonad m, Prim a, HasCallStack)
                          => Int -> m (A.MutablePrimArray (PrimState m) a)
{-# INLINE newAlignedPinnedPrimArray #-}
newAlignedPinnedPrimArray n =
#ifdef CHECK_ARRAY_BOUND
    check  (n>=0) (A.newAlignedPinnedPrimArray n)
#else
    (A.newAlignedPinnedPrimArray n)
#endif

copyPrimArrayToPtr :: (PrimMonad m, Prim a, HasCallStack)
                   => Ptr a
                   -> A.PrimArray a
                   -> Int
                   -> Int
                   -> m ()
{-# INLINE copyPrimArrayToPtr #-}
copyPrimArrayToPtr ptr arr s l =
#ifdef CHECK_ARRAY_BOUND
    check
        (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
        (A.copyPrimArrayToPtr ptr arr s l)
#else
    (A.copyPrimArrayToPtr ptr arr s l)
#endif

copyMutablePrimArrayToPtr :: (PrimMonad m, Prim a, HasCallStack)
                          => Ptr a
                          -> A.MutablePrimArray (PrimState m) a
                          -> Int
                          -> Int
                          -> m ()
{-# INLINE copyMutablePrimArrayToPtr #-}
copyMutablePrimArrayToPtr ptr marr s l = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.copyMutablePrimArrayToPtr ptr marr s l)
#else
    (A.copyMutablePrimArrayToPtr ptr marr s l)
#endif

copyPtrToMutablePrimArray :: (PrimMonad m, Prim a, HasCallStack)
                            => A.MutablePrimArray (PrimState m) a
                            -> Int
                            -> Ptr a
                            -> Int
                            -> m ()
{-# INLINE copyPtrToMutablePrimArray #-}
copyPtrToMutablePrimArray marr s ptr l = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.copyPtrToMutablePrimArray marr s ptr l)
#else
        (A.copyPtrToMutablePrimArray marr s ptr l)
#endif

--------------------------------------------------------------------------------

modifyIndexArr :: (Arr arr a, HasCallStack) => arr a
               -> Int    -- ^ offset
               -> Int    -- ^ length
               -> Int    -- ^ index in new array
               -> (a -> a)   -- ^ modify function
               -> arr a
{-# INLINE modifyIndexArr #-}
modifyIndexArr arr off len ix f =
#ifdef CHECK_ARRAY_BOUND
    runST $ do
        marr <- A.unsafeThawArr (cloneArr arr off len)
        !v <- f <$> readArr marr ix
        writeArr marr ix v
        A.unsafeFreezeArr marr
#else
    A.modifyIndexArr arr off len ix f
#endif

-- | Insert an immutable array's element at given index to produce a new array.
insertIndexArr :: Arr arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ insert index in new array
               -> a          -- ^ element to be inserted
               -> arr a
{-# INLINE insertIndexArr #-}
insertIndexArr arr s l i x =
#ifdef CHECK_ARRAY_BOUND
    runST $ do
        marr <- newArrWith (l+1) x
        when (i>0) $ copyArr marr 0 arr s i
        when (i<l) $ copyArr marr (i+1) arr (i+s) (l-i)
        A.unsafeFreezeArr marr
#else
    A.insertIndexArr arr s l i x
#endif

-- | Drop an immutable array's element at given index to produce a new array.
deleteIndexArr :: Arr arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ drop index in new array
               -> arr a
{-# INLINE deleteIndexArr #-}
deleteIndexArr arr s l i =
#ifdef CHECK_ARRAY_BOUND
    runST $ do
        marr <- newArr (l-1)
        when (i>0) $ copyArr marr 0 arr s i
        let i' = i+1
        when (i'<l) $ copyArr marr i arr (i'+s) (l-i')
        A.unsafeFreezeArr marr
#else
    A.deleteIndexArr arr s l i
#endif

-- | Swap two elements under given index and return a new array.
swapArr :: Arr arr a
        => arr a
        -> Int 
        -> Int
        -> arr a
{-# INLINE swapArr #-}
swapArr arr i j = runST $ do
    marr <- A.thawArr arr 0 (A.sizeofArr arr)
    swapMutableArr marr i j
    A.unsafeFreezeArr marr

-- | Swap two elements under given index, no atomically guarantee is given.
swapMutableArr :: (PrimMonad m, PrimState m ~ s, Arr arr a)
               => MArr arr s a
               -> Int 
               -> Int
               -> m ()
{-# INLINE swapMutableArr #-}
swapMutableArr marr i j = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (i>=0 && j>=0 && i<siz && j<siz)
        (A.swapMutableArr marr i j)
#else
    A.swapMutableArr marr i j
#endif

-- | Shuffle array's elements in slice range.
--
-- This function use <https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates> algorithm. 
shuffleMutableArr :: (StatefulGen g m, PrimMonad m, PrimState m ~ s, Arr arr a) => g -> MArr arr s a 
            -> Int  -- ^ offset
            -> Int  -- ^ length
            -> m ()
{-# INLINE shuffleMutableArr #-}
shuffleMutableArr g marr s l = do
#ifdef CHECK_ARRAY_BOUND
    siz <- A.sizeofMutableArr marr
    check
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.shuffleMutableArr g marr s l)
#else
        A.shuffleMutableArr g marr s l
#endif