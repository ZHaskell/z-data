{-|
Module      : Z.Data.Array
Description : Fast boxed and unboxed arrays
Copyright   : (c) Dong Han, 2017
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Unified unboxed and boxed array operations using type family.

NONE of the operations are bound checked, if you need checked operations please use "Z.Data.Array.Checked" instead.
It exports the exact same APIs ,so it requires no extra effort to switch between them.

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

module Z.Data.Array.Base (
  -- * Arr typeclass
    Arr(..)
  , emptyArr, singletonArr, doubletonArr
  , modifyIndexArr, insertIndexArr, deleteIndexArr, swapArr, swapMutableArr
  , doubleMutableArr, shuffleMutableArr
  , RealWorld
  -- * Boxed array type
  , Array(..)
  , MutableArray(..)
  , SmallArray(..)
  , SmallMutableArray(..)
  , uninitialized
  -- * Primitive array type
  , PrimArray(..)
  , MutablePrimArray(..)
  , Prim(..)
  -- * Primitive array operations
  , newPinnedPrimArray, newAlignedPinnedPrimArray
  , copyPrimArrayToPtr, copyMutablePrimArrayToPtr, copyPtrToMutablePrimArray
  , primArrayContents, mutablePrimArrayContents, withPrimArrayContents, withMutablePrimArrayContents
  , isPrimArrayPinned, isMutablePrimArrayPinned
  -- * Unlifted array type
  , UnliftedArray(..)
  , MutableUnliftedArray(..)
  , PrimUnlifted(..)
  -- * The 'ArrayException' type
  , ArrayException(..)
  -- * Cast between primitive arrays
  , Cast
  , castArray
  , castMutableArray
  -- * Re-export
  , sizeOf
  ) where

import           Control.Exception              (ArrayException (..), throw)
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits                      (unsafeShiftL)
import           Data.Kind                      (Type)
import           Data.Primitive.Array
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Primitive.Ptr             (copyPtrToMutablePrimArray)
import           Data.Primitive.SmallArray
import           Data.Primitive.Types
import           GHC.Exts
import           System.Random.Stateful  ( UniformRange(uniformRM), StatefulGen )  
import           Z.Data.Array.Cast
import           Z.Data.Array.UnliftedArray


-- | Bottom value (@throw ('UndefinedElement' 'Data.Array.uninitialized')@)
-- for new boxed array('Array', 'SmallArray'..) initialization.
--
uninitialized :: a
uninitialized = throw (UndefinedElement "Data.Array.uninitialized")


-- | The typeclass that unifies box & unboxed and mutable & immutable array operations.
--
-- Most of these functions simply wrap their primitive counterpart.
-- When there are no primitive ones, we fulfilled the semantic with other operations.
--
-- One exception is 'shrinkMutableArr' which only performs closure resizing on 'PrimArray', because
-- currently, RTS only supports that. 'shrinkMutableArr' won't do anything on other array types.
--
-- It's reasonable to trust GHC to specialize & inline these polymorphic functions.
-- They are used across this package and perform identically to their monomorphic counterpart.
--
class Arr (arr :: Type -> Type) a where


    -- | The mutable version of this array type.
    --
    type MArr arr = (mar :: Type -> Type -> Type) | mar -> arr


    -- | Make a new array with a given size.
    --
    -- For boxed arrays, all elements are 'uninitialized' , which shall not be accessed.
    -- For primitive arrays, elements are just random garbage.
    newArr :: (PrimMonad m, PrimState m ~ s) => Int -> m (MArr arr s a)


    -- | Make a new array and fill it with an initial value.
    newArrWith :: (PrimMonad m, PrimState m ~ s) => Int -> a -> m (MArr arr s a)


    -- | Read from specified index of mutable array in a primitive monad.
    readArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> Int -> m a


    -- | Write to specified index of mutable array in a primitive monad.
    writeArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> Int -> a -> m ()


    -- | Fill the mutable array with a given value.
    setArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> Int -> Int -> a -> m ()


    -- | Read from the specified index of an immutable array. It's pure and often
    -- results in an indexing thunk for lifted arrays, use 'indexArr\'' or 'indexArrM' to avoid this.
    indexArr :: arr a -> Int -> a


    -- | Read from the specified index of an immutable array. The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
    -- Pattern matching on the tuple forces the indexing of the array to happen but does not evaluate the element itself.
    -- Evaluating the thunk prevents additional thunks from building up on the heap.
    -- Avoiding these thunks, in turn, reduces references to the argument array, allowing it to be garbage collected more promptly.
    indexArr' :: arr a -> Int -> (# a #)


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
    indexArrM :: (Monad m) => arr a -> Int -> m a


    -- | Create an immutable copy of a slice of an array.
    -- This operation makes a copy of the specified section, so it is safe to continue using the mutable array afterward.
    freezeArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> Int -> Int -> m (arr a)


    -- | Create a mutable array from a slice of an immutable array.
    -- This operation makes a copy of the specified slice, so it is safe to use the immutable array afterward.
    thawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (MArr arr s a)


    -- | Convert a mutable array to an immutable one without copying.
    -- The array should not be modified after the conversion.
    unsafeFreezeArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> m (arr a)



    -- | Convert a mutable array to an immutable one without copying. The
    -- array should not be modified after the conversion.
    unsafeThawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> m (MArr arr s a)


    -- | Copy a slice of an immutable array to a mutable array at given offset.
    copyArr ::  (PrimMonad m, PrimState m ~ s)
            => MArr arr s a -- ^ target
            -> Int          -- ^ offset into target array
            -> arr a        -- ^ source
            -> Int          -- ^ offset into source array
            -> Int          -- ^ number of elements to copy
            -> m ()


    -- | Copy a slice of a mutable array to another mutable array at given offset.
    -- The two mutable arrays must not be the same.
    copyMutableArr :: (PrimMonad m, PrimState m ~ s)
                   => MArr arr s a  -- ^ target
                   -> Int           -- ^ offset into target array
                   -> MArr arr s a  -- ^ source
                   -> Int           -- ^ offset into source array
                   -> Int           -- ^ number of elements to copy
                   -> m ()


    -- | Copy a slice of a mutable array to a mutable array at given offset.
    -- The two mutable arrays can be the same.
    moveArr :: (PrimMonad m, PrimState m ~ s)
            => MArr arr s a  -- ^ target
            -> Int           -- ^ offset into target array
            -> MArr arr s a  -- ^ source
            -> Int           -- ^ offset into source array
            -> Int           -- ^ number of elements to copy
            -> m ()


    -- | Create an immutable copy with the given subrange of the original array.
    cloneArr :: arr a -> Int -> Int -> arr a


    -- | Create a mutable copy the given subrange of the original array.
    cloneMutableArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> Int -> Int -> m (MArr arr s a)


    -- | Resize a mutable array to the given size.
    resizeMutableArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> Int -> m (MArr arr s a)


    -- | Shrink a mutable array to the given size. This operation only works on primitive arrays.
    -- For some array types, this is a no-op, e.g. 'sizeOfMutableArr' will not change.
    shrinkMutableArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> Int -> m ()


    -- | Is two mutable array are reference equal.
    sameMutableArr :: MArr arr s a -> MArr arr s a -> Bool


    -- | Size of the immutable array.
    sizeofArr :: arr a -> Int


    -- | Size of the mutable array.
    sizeofMutableArr :: (PrimMonad m, PrimState m ~ s) => MArr arr s a -> m Int


    -- | Check whether the two immutable arrays refer to the same memory block
    --
    -- Note that the result of 'sameArr' may change depending on compiler's optimizations, for example,
    -- @let arr = runST ... in arr `sameArr` arr@ may return false if compiler decides to
    -- inline it.
    --
    -- See https://ghc.haskell.org/trac/ghc/ticket/13908 for more context.
    --
    sameArr :: arr a -> arr a -> Bool

instance Arr Array a where
    type MArr Array = MutableArray
    newArr n = newArray n uninitialized
    {-# INLINE newArr #-}
    newArrWith = newArray
    {-# INLINE newArrWith #-}
    readArr = readArray
    {-# INLINE readArr #-}
    writeArr = writeArray
    {-# INLINE writeArr #-}
    setArr marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr = indexArray
    {-# INLINE indexArr #-}
    indexArr' (Array arr#) (I# i#) = indexArray# arr# i#
    {-# INLINE indexArr' #-}
    indexArrM = indexArrayM
    {-# INLINE indexArrM #-}
    freezeArr = freezeArray
    {-# INLINE freezeArr #-}
    thawArr = thawArray
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawArray
    {-# INLINE unsafeThawArr #-}

    copyArr = copyArray
    {-# INLINE copyArr #-}
    copyMutableArr = copyMutableArray
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableArray marr1 marr2 =
            case compare s1 s2 of
                LT ->
                    let !d = s2 - s1
                        !s2l = s2 + l
                        go !i | i >= s2l = return ()
                              | otherwise = do x <- readArray marr2 i
                                               writeArray marr1 (i-d) x
                                               go (i+1)
                    in go s2

                EQ -> return ()

                GT ->
                    let !d = s1 - s2
                        go !i | i < s2 = return ()
                              | otherwise = do x <- readArray marr2 i
                                               writeArray marr1 (i+d) x
                                               go (i-1)
                    in go (s2+l-1)
        | otherwise = copyMutableArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr = cloneArray
    {-# INLINE cloneArr #-}
    cloneMutableArr = cloneMutableArray
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = do
        marr' <- newArray n uninitialized
        copyMutableArray marr' 0 marr 0 (sizeofMutableArray marr)
        return marr'
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutableArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofMutableArray
    {-# INLINE sizeofMutableArr #-}

    sameArr (Array arr1#) (Array arr2#) = isTrue# (
        sameMutableArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArr #-}

instance Arr SmallArray a where
    type MArr SmallArray = SmallMutableArray
    newArr n = newSmallArray n uninitialized
    {-# INLINE newArr #-}
    newArrWith = newSmallArray
    {-# INLINE newArrWith #-}
    readArr = readSmallArray
    {-# INLINE readArr #-}
    writeArr = writeSmallArray
    {-# INLINE writeArr #-}
    setArr marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeSmallArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr = indexSmallArray
    {-# INLINE indexArr #-}
    indexArr' (SmallArray arr#) (I# i#) = indexSmallArray# arr# i#
    {-# INLINE indexArr' #-}
    indexArrM = indexSmallArrayM
    {-# INLINE indexArrM #-}
    freezeArr = freezeSmallArray
    {-# INLINE freezeArr #-}
    thawArr = thawSmallArray
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeSmallArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawSmallArray
    {-# INLINE unsafeThawArr #-}

    copyArr = copySmallArray
    {-# INLINE copyArr #-}
    copyMutableArr = copySmallMutableArray
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableArr marr1 marr2 =
            case compare s1 s2 of
                LT ->
                    let !d = s2 - s1
                        !s2l = s2 + l
                        go !i | i >= s2l = return ()
                              | otherwise = do x <- readSmallArray marr2 i
                                               writeSmallArray marr1 (i-d) x
                                               go (i+1)
                    in go s2

                EQ -> return ()

                GT ->
                    let !d = s1 - s2
                        go !i | i < s2 = return ()
                              | otherwise = do x <- readSmallArray marr2 i
                                               writeSmallArray marr1 (i+d) x
                                               go (i-1)
                    in go (s2+l-1)
        | otherwise = copySmallMutableArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr = cloneSmallArray
    {-# INLINE cloneArr #-}
    cloneMutableArr = cloneSmallMutableArray
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = do
        marr' <- newSmallArray n uninitialized
        copySmallMutableArray marr' 0 marr 0 (sizeofSmallMutableArray marr)
        return marr'
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr = shrinkSmallMutableArray
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr (SmallMutableArray smarr1#) (SmallMutableArray smarr2#) =
        isTrue# (sameSmallMutableArray# smarr1# smarr2#)
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofSmallArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofSmallMutableArray
    {-# INLINE sizeofMutableArr #-}

    sameArr (SmallArray arr1#) (SmallArray arr2#) = isTrue# (
        sameSmallMutableArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArr #-}

instance Prim a => Arr PrimArray a where
    type MArr PrimArray = MutablePrimArray
    newArr = newPrimArray
    {-# INLINE newArr #-}
    newArrWith n x = do
        marr <- newPrimArray n
        when (n > 0) (setPrimArray marr 0 n x)
        return marr
    {-# INLINE newArrWith #-}
    readArr = readPrimArray
    {-# INLINE readArr #-}
    writeArr = writePrimArray
    {-# INLINE writeArr #-}
    setArr = setPrimArray
    {-# INLINE setArr #-}
    indexArr = indexPrimArray
    {-# INLINE indexArr #-}
    indexArr' arr i = (# indexPrimArray arr i #)
    {-# INLINE indexArr' #-}
    indexArrM arr i = return (indexPrimArray arr i)
    {-# INLINE indexArrM #-}
    freezeArr = freezePrimArray
    {-# INLINE freezeArr #-}
    thawArr arr s l = do
        marr' <- newPrimArray l
        copyPrimArray marr' 0 arr s l
        return marr'
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezePrimArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawPrimArray
    {-# INLINE unsafeThawArr #-}

    copyArr = copyPrimArray
    {-# INLINE copyArr #-}
    copyMutableArr = copyMutablePrimArray
    {-# INLINE copyMutableArr #-}

    moveArr (MutablePrimArray dst) doff (MutablePrimArray src) soff n =
        moveByteArray (MutableByteArray dst) (doff*siz) (MutableByteArray src) (soff*siz) (n*siz)
      where siz = sizeOf (undefined :: a)
    {-# INLINE moveArr #-}

    cloneArr = clonePrimArray
    {-# INLINE cloneArr #-}
    cloneMutableArr = cloneMutablePrimArray
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr = resizeMutablePrimArray
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr = shrinkMutablePrimArray
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutablePrimArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofPrimArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = getSizeofMutablePrimArray
    {-# INLINE sizeofMutableArr #-}

    sameArr (PrimArray ba1#) (PrimArray ba2#) =
        isTrue# (sameMutableByteArray# (unsafeCoerce# ba1#) (unsafeCoerce# ba2#))
    {-# INLINE sameArr #-}

instance PrimUnlifted a => Arr UnliftedArray a where
    type MArr UnliftedArray = MutableUnliftedArray
    newArr = unsafeNewUnliftedArray
    {-# INLINE newArr #-}
    newArrWith = newUnliftedArray
    {-# INLINE newArrWith #-}
    readArr = readUnliftedArray
    {-# INLINE readArr #-}
    writeArr = writeUnliftedArray
    {-# INLINE writeArr #-}
    setArr = setUnliftedArray
    {-# INLINE setArr #-}
    indexArr = indexUnliftedArray
    {-# INLINE indexArr #-}
    indexArr' arr i = (# indexUnliftedArray arr i #)
    {-# INLINE indexArr' #-}
    indexArrM arr i = return (indexUnliftedArray arr i)
    {-# INLINE indexArrM #-}
    freezeArr = freezeUnliftedArray
    {-# INLINE freezeArr #-}
    thawArr = thawUnliftedArray
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeUnliftedArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr (UnliftedArray arr#) = primitive ( \ s0# ->
            let !(# s1#, marr# #) = unsafeThawArray# (unsafeCoerce# arr#) s0#
                                                        -- ArrayArray# and Array# use the same representation
            in (# s1#, MutableUnliftedArray (unsafeCoerce# marr#) #)    -- so this works
        )
    {-# INLINE unsafeThawArr #-}

    copyArr = copyUnliftedArray
    {-# INLINE copyArr #-}
    copyMutableArr = copyMutableUnliftedArray
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableUnliftedArray marr1 marr2 =
            case compare s1 s2 of
                LT ->
                    let !d = s2 - s1
                        !s2l = s2 + l
                        go !i | i >= s2l = return ()
                              | otherwise = do x <- readUnliftedArray marr2 i
                                               writeUnliftedArray marr1 (i-d) x
                                               go (i+1)
                    in go s2

                EQ -> return ()

                GT ->
                    let !d = s1 - s2
                        go !i | i < s2 = return ()
                              | otherwise = do x <- readUnliftedArray marr2 i
                                               writeUnliftedArray marr1 (i+d) x
                                               go (i-1)
                    in go (s2+l-1)
        | otherwise = copyMutableUnliftedArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr = cloneUnliftedArray
    {-# INLINE cloneArr #-}
    cloneMutableArr = cloneMutableUnliftedArray
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = do
        marr' <- newUnliftedArray n uninitialized
        copyMutableUnliftedArray marr' 0 marr 0 (sizeofMutableUnliftedArray marr)
        return marr'
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutableUnliftedArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofUnliftedArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofMutableUnliftedArray
    {-# INLINE sizeofMutableArr #-}

    sameArr (UnliftedArray arr1#) (UnliftedArray arr2#) = isTrue# (
        sameMutableArrayArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArr #-}

--------------------------------------------------------------------------------

-- | Obtain the pointer to the content of an array, and the pointer should only be used during the IO action.
--
-- This operation is only safe on /pinned/ primitive arrays (Arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray').
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimArrayContents :: PrimArray a -> (Ptr a -> IO b) -> IO b
{-# INLINE withPrimArrayContents #-}
withPrimArrayContents (PrimArray ba#) f = do
    let addr# = byteArrayContents# ba#
        ptr = Ptr addr#
    b <- f ptr
    primitive_ (touch# ba#)
    return b

-- | Obtain the pointer to the content of an mutable array, and the pointer should only be used during the IO action.
--
-- This operation is only safe on /pinned/ primitive arrays (Arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray').
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withMutablePrimArrayContents :: MutablePrimArray RealWorld a -> (Ptr a -> IO b) -> IO b
{-# INLINE withMutablePrimArrayContents #-}
withMutablePrimArrayContents (MutablePrimArray mba#) f = do
    let addr# = byteArrayContents# (unsafeCoerce# mba#)
        ptr = Ptr addr#
    b <- f ptr
    primitive_ (touch# mba#)
    return b

-- | Cast between arrays
castArray :: (Arr arr a, Cast a b) => arr a -> arr b
{-# INLINE castArray #-}
castArray = unsafeCoerce#


-- | Cast between mutable arrays
castMutableArray :: (Arr arr a, Cast a b) => MArr arr s a -> MArr arr s b
{-# INLINE castMutableArray #-}
castMutableArray = unsafeCoerce#

--------------------------------------------------------------------------------

emptyArr :: Arr arr a => arr a
{-# NOINLINE emptyArr #-}
emptyArr = runST $ do
    marr <- newArrWith 0 uninitialized
    unsafeFreezeArr marr

singletonArr :: Arr arr a => a -> arr a
{-# INLINE singletonArr #-}
singletonArr x = runST $ do
    marr <- newArrWith 1 x
    unsafeFreezeArr marr

doubletonArr :: Arr arr a => a -> a -> arr a
{-# INLINE doubletonArr #-}
doubletonArr x y = runST $ do
    marr <- newArrWith 2 x
    writeArr marr 1 y
    unsafeFreezeArr marr

-- | Modify(strictly) an immutable some elements of an array with specified subrange.
-- This function will produce a new array.
modifyIndexArr :: Arr arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ index in new array
               -> (a -> a)   -- ^ modify function
               -> arr a
{-# INLINE modifyIndexArr #-}
modifyIndexArr arr off len ix f = runST $ do
    marr <- unsafeThawArr (cloneArr arr off len)
    !v <- f <$> readArr marr ix
    writeArr marr ix v
    unsafeFreezeArr marr

-- | Insert a value to an immutable array at given index. This function will produce a new array.
insertIndexArr :: Arr arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ insert index in new array
               -> a          -- ^ value to be inserted
               -> arr a
{-# INLINE insertIndexArr #-}
insertIndexArr arr s l i x = runST $ do
    marr <- newArrWith (l+1) x
    when (i>0) $ copyArr marr 0 arr s i
    when (i<l) $ copyArr marr (i+1) arr (i+s) (l-i)
    unsafeFreezeArr marr

-- | Delete an element of the immutable array's at given index. This function will produce a new array.
deleteIndexArr :: Arr arr a
               => arr a
               -> Int        -- ^ offset
               -> Int        -- ^ length
               -> Int        -- ^ the index of the element to delete
               -> arr a
{-# INLINE deleteIndexArr #-}
deleteIndexArr arr s l i = runST $ do
    marr <- newArr (l-1)
    when (i>0) $ copyArr marr 0 arr s i
    let i' = i+1
    when (i'<l) $ copyArr marr i arr (i'+s) (l-i')
    unsafeFreezeArr marr

-- | Swap two elements under given index and return a new array.
swapArr :: Arr arr a
             => arr a
             -> Int 
             -> Int
             -> arr a
{-# INLINE swapArr #-}
swapArr arr i j = runST $ do
    marr <- thawArr arr 0 (sizeofArr arr)
    swapMutableArr marr i j
    unsafeFreezeArr marr

-- | Swap two elements under given index, no atomically guarantee is given.
swapMutableArr :: (PrimMonad m, PrimState m ~ s, Arr arr a)
             => MArr arr s a
             -> Int 
             -> Int
             -> m ()
{-# INLINE swapMutableArr #-}
swapMutableArr marr i j = do
    x <- readArr marr i
    y <- readArr marr j
    writeArr marr i y 
    writeArr marr j x 

-- | Resize mutable array to @max (given_size) (2 * original_size)@ if orignal array is smaller than @give_size@.
doubleMutableArr :: (Arr arr a, PrimMonad m, PrimState m ~ s) => MArr arr s a -> Int -> m (MArr arr s a)
{-# INLINE doubleMutableArr #-}
doubleMutableArr marr l = do
    siz <- sizeofMutableArr marr
    if (siz < l)
    then resizeMutableArr marr (max (siz `unsafeShiftL` 1) l)
    else return marr


-- | Shuffle array's elements in slice range.
--
-- This function use <https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates> algorithm. 
shuffleMutableArr :: (StatefulGen g m, PrimMonad m, PrimState m ~ s, Arr arr a) => g -> MArr arr s a 
            -> Int  -- ^ offset
            -> Int  -- ^ length
            -> m ()
{-# INLINE shuffleMutableArr #-}
shuffleMutableArr g marr off n = go (off+n-1)
  where 
    go i | i < off+1 = return ()
         | otherwise = do
            j <- uniformRM (off, i) g
            swapMutableArr marr i j 
            go (i - 1) 
