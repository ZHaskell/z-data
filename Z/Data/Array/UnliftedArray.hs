{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}

-- GHC contains three general classes of value types:
--
--   1. Unboxed types: values are machine values made up of fixed numbers of bytes
--   2. Unlifted types: values are pointers, but strictly evaluated
--   3. Lifted types: values are pointers, lazily evaluated
--
-- The first category can be stored in a 'ByteArray', and this allows types in
-- category 3 that are simple wrappers around category 1 types to be stored
-- more efficiently using a 'ByteArray'. This module provides the same facility
-- for category 2 types.
--
-- GHC has two primitive types, 'ArrayArray#' and 'MutableArrayArray#'. These
-- are arrays of pointers, but of category 2 values, so they are known to not
-- be bottom. This allows types that are wrappers around such types to be stored
-- in an array without an extra level of indirection.
--
-- The way that the 'ArrayArray#' API works is that one can read and write
-- 'ArrayArray#' values to the positions. This works because all category 2
-- types share a uniform representation, unlike unboxed values which are
-- represented by varying (by type) numbers of bytes. However, using the
-- this makes the internal API very unsafe to use, as one has to coerce values
-- to and from 'ArrayArray#'.
--
-- The API presented by this module is more type safe. 'UnliftedArray' and
-- 'MutableUnliftedArray' are parameterized by the type of arrays they contain, and
-- the coercions necessary are abstracted into a class, 'PrimUnlifted', of things
-- that are eligible to be stored.
--
module Z.Data.Array.UnliftedArray where

import Control.Monad.Primitive
import Data.Primitive.PrimArray (PrimArray(..),MutablePrimArray(..))
import Data.Primitive.ByteArray (ByteArray(..),MutableByteArray(..))
import GHC.MVar (MVar(..))
import GHC.IORef (IORef(..))
import GHC.STRef (STRef(..))
import GHC.Exts
import GHC.IO.Unsafe

class PrimUnlifted a where
    writeUnliftedArray# :: MutableArrayArray# s -> Int# -> a -> State# s -> State# s
    readUnliftedArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, a #)
    indexUnliftedArray# :: ArrayArray# -> Int# -> a

instance PrimUnlifted (PrimArray a) where
    {-# inline writeUnliftedArray# #-}
    {-# inline readUnliftedArray# #-}
    {-# inline indexUnliftedArray# #-}
    writeUnliftedArray# a i (PrimArray x) = writeByteArrayArray# a i x
    readUnliftedArray# a i s0 = case readByteArrayArray# a i s0 of
        (# s1, x #) -> (# s1, PrimArray x #)
    indexUnliftedArray# a i = PrimArray (indexByteArrayArray# a i)

instance PrimUnlifted ByteArray where
    {-# inline writeUnliftedArray# #-}
    {-# inline readUnliftedArray# #-}
    {-# inline indexUnliftedArray# #-}
    writeUnliftedArray# a i (ByteArray x) = writeByteArrayArray# a i x
    readUnliftedArray# a i s0 = case readByteArrayArray# a i s0 of
        (# s1, x #) -> (# s1, ByteArray x #)
    indexUnliftedArray# a i = ByteArray (indexByteArrayArray# a i)

-- This uses unsafeCoerce# in the implementation of
-- indexUnliftedArray#. This does not lead to corruption FFI codegen
-- since ByteArray# and MutableByteArray# have the same FFI offset
-- applied by add_shim.
-- This also uses unsafeCoerce# to relax the constraints on the
-- state token. The primitives in GHC.Prim are too restrictive.
instance PrimUnlifted (MutableByteArray s) where
    {-# inline writeUnliftedArray# #-}
    {-# inline readUnliftedArray# #-}
    {-# inline indexUnliftedArray# #-}
    writeUnliftedArray# a i (MutableByteArray x) =
        writeMutableByteArrayArray# a i (unsafeCoerce# x)
    readUnliftedArray# a i s0 = case readMutableByteArrayArray# a i s0 of
        (# s1, x #) -> (# s1, MutableByteArray (unsafeCoerce# x) #)
    indexUnliftedArray# a i = MutableByteArray (unsafeCoerce# (indexByteArrayArray# a i))

-- See the note on the PrimUnlifted instance for MutableByteArray.
-- The same uses of unsafeCoerce# happen here.
instance PrimUnlifted (MutablePrimArray s a) where
    {-# inline writeUnliftedArray# #-}
    {-# inline readUnliftedArray# #-}
    {-# inline indexUnliftedArray# #-}
    writeUnliftedArray# a i (MutablePrimArray x) =
        writeMutableByteArrayArray# a i (unsafeCoerce# x)
    readUnliftedArray# a i s0 = case readMutableByteArrayArray# a i s0 of
        (# s1, x #) -> (# s1, MutablePrimArray (unsafeCoerce# x) #)
    indexUnliftedArray# a i = MutablePrimArray (unsafeCoerce# (indexByteArrayArray# a i))

-- This uses unsafeCoerce# in the implementation of all of its
-- methods. See the note for the PrimUnlifted instance of
-- Data.Primitive.MVar.MVar.
instance PrimUnlifted (MVar a) where
    {-# inline writeUnliftedArray# #-}
    {-# inline readUnliftedArray# #-}
    {-# inline indexUnliftedArray# #-}
    writeUnliftedArray# a i (MVar x) =
        writeArrayArrayArray# a i (unsafeCoerce# x)
    readUnliftedArray# a i s0 = case readArrayArrayArray# a i s0 of
        (# s1, x #) -> (# s1, MVar (unsafeCoerce# x) #)
    indexUnliftedArray# a i = MVar (unsafeCoerce# (indexArrayArrayArray# a i))

-- This uses unsafeCoerce# in the implementation of all of its
-- methods. This does not lead to corruption FFI codegen since ArrayArray#
-- and MutVar# have the same FFI offset applied by add_shim.
instance PrimUnlifted (STRef s a) where
    {-# inline writeUnliftedArray# #-}
    {-# inline readUnliftedArray# #-}
    {-# inline indexUnliftedArray# #-}
    writeUnliftedArray# a i (STRef x) =
        writeArrayArrayArray# a i (unsafeCoerce# x)
    readUnliftedArray# a i s0 = case readArrayArrayArray# a i s0 of
        (# s1, x #) -> (# s1, STRef (unsafeCoerce# x) #)
    indexUnliftedArray# a i =
        STRef (unsafeCoerce# (indexArrayArrayArray# a i))

instance PrimUnlifted (IORef a) where
    {-# inline writeUnliftedArray# #-}
    {-# inline readUnliftedArray# #-}
    {-# inline indexUnliftedArray# #-}
    writeUnliftedArray# a i (IORef v) = writeUnliftedArray# a i v
    readUnliftedArray# a i s0 = case readUnliftedArray# a i s0 of
        (# s1, v #) -> (# s1, IORef v #)
    indexUnliftedArray# a i = IORef (indexUnliftedArray# a i)

--------------------------------------------------------------------------------

data MutableUnliftedArray s a
    = MutableUnliftedArray (MutableArrayArray# s)

data UnliftedArray a
    = UnliftedArray ArrayArray#

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the array itself. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from the @'MutableUnliftedArray' s a@ to the element type.
unsafeNewUnliftedArray
    :: (PrimMonad m)
    => Int -- ^ size
    -> m (MutableUnliftedArray (PrimState m) a)
{-# inline unsafeNewUnliftedArray #-}
unsafeNewUnliftedArray (I# i#) = primitive $ \s -> case newArrayArray# i# s of
    (# s', maa# #) -> (# s', MutableUnliftedArray maa# #)

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents. This is slower than 'unsafeNewUnliftedArray', but safer.
newUnliftedArray
    :: (PrimMonad m, PrimUnlifted a)
    => Int -- ^ size
    -> a -- ^ initial value
    -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray len v = do
    mua <- unsafeNewUnliftedArray len
    setUnliftedArray mua 0 len v
    pure mua
{-# inline newUnliftedArray #-}

setUnliftedArray
    :: (PrimMonad m, PrimUnlifted a)
    => MutableUnliftedArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset
    -> Int -- ^ length
    -> a -- ^ value to fill with
    -> m ()
{-# inline setUnliftedArray #-}
setUnliftedArray mua off len v = loop (len + off - 1)
  where
    loop i
        | i < off = pure ()
        | otherwise = writeUnliftedArray mua i v *> loop (i-1)

-- | Yields the length of an 'UnliftedArray'.
sizeofUnliftedArray :: UnliftedArray e -> Int
{-# inline sizeofUnliftedArray #-}
sizeofUnliftedArray (UnliftedArray aa#) = I# (sizeofArrayArray# aa#)

-- | Yields the length of a 'MutableUnliftedArray'.
sizeofMutableUnliftedArray :: MutableUnliftedArray s e -> Int
{-# inline sizeofMutableUnliftedArray #-}
sizeofMutableUnliftedArray (MutableUnliftedArray maa#)
    = I# (sizeofMutableArrayArray# maa#)

writeUnliftedArray :: (PrimMonad m, PrimUnlifted a)
    => MutableUnliftedArray (PrimState m) a
    -> Int
    -> a
    -> m ()
{-# inline writeUnliftedArray #-}
writeUnliftedArray (MutableUnliftedArray arr) (I# ix) a =
    primitive_ (writeUnliftedArray# arr ix a)

readUnliftedArray :: (PrimMonad m, PrimUnlifted a)
    => MutableUnliftedArray (PrimState m) a
    -> Int
    -> m a
{-# inline readUnliftedArray #-}
readUnliftedArray (MutableUnliftedArray arr) (I# ix) =
    primitive (readUnliftedArray# arr ix)

indexUnliftedArray :: PrimUnlifted a
    => UnliftedArray a
    -> Int
    -> a
{-# inline indexUnliftedArray #-}
indexUnliftedArray (UnliftedArray arr) (I# ix) =
    indexUnliftedArray# arr ix

-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a
    -> m (UnliftedArray a)
unsafeFreezeUnliftedArray (MutableUnliftedArray maa#)
    = primitive $ \s -> case unsafeFreezeArrayArray# maa# s of
        (# s', aa# #) -> (# s', UnliftedArray aa# #)
{-# inline unsafeFreezeUnliftedArray #-}

-- | Determines whether two 'MutableUnliftedArray' values are the same. This is
-- object/pointer identity, not based on the contents.
sameMutableUnliftedArray
    :: MutableUnliftedArray s a
    -> MutableUnliftedArray s a
    -> Bool
sameMutableUnliftedArray (MutableUnliftedArray maa1#) (MutableUnliftedArray maa2#)
    = isTrue# (sameMutableArrayArray# maa1# maa2#)
{-# inline sameMutableUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset into destination
    -> UnliftedArray a -- ^ source
    -> Int -- ^ offset into source
    -> Int -- ^ number of elements to copy
    -> m ()
{-# inline copyUnliftedArray #-}
copyUnliftedArray
    (MutableUnliftedArray dst) (I# doff)
    (UnliftedArray src) (I# soff) (I# ln) =
      primitive_ $ copyArrayArray# src soff dst doff ln


-- | Copies the contents of one mutable array into another.
copyMutableUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset into destination
    -> MutableUnliftedArray (PrimState m) a -- ^ source
    -> Int -- ^ offset into source
    -> Int -- ^ number of elements to copy
    -> m ()
{-# inline copyMutableUnliftedArray #-}
copyMutableUnliftedArray
    (MutableUnliftedArray dst) (I# doff)
    (MutableUnliftedArray src) (I# soff) (I# ln) =
      primitive_ $ copyMutableArrayArray# src soff dst doff ln


-- | Freezes a portion of a 'MutableUnliftedArray', yielding an 'UnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (UnliftedArray a)
freezeUnliftedArray src off len = do
    dst <- unsafeNewUnliftedArray len
    copyMutableUnliftedArray dst 0 src off len
    unsafeFreezeUnliftedArray dst
{-# inline freezeUnliftedArray #-}


-- | Thaws a portion of an 'UnliftedArray', yielding a 'MutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawUnliftedArray
    :: (PrimMonad m)
    => UnliftedArray a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (MutableUnliftedArray (PrimState m) a)
{-# inline thawUnliftedArray #-}
thawUnliftedArray src off len = do
    dst <- unsafeNewUnliftedArray len
    copyUnliftedArray dst 0 src off len
    return dst

-- | Creates a copy of a portion of an 'UnliftedArray'
cloneUnliftedArray
    :: UnliftedArray a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> UnliftedArray a
{-# inline cloneUnliftedArray #-}
cloneUnliftedArray src off len = unsafeDupablePerformIO $ do
    dst <- unsafeNewUnliftedArray len
    copyUnliftedArray dst 0 src off len
    unsafeFreezeUnliftedArray dst

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneMutableUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (MutableUnliftedArray (PrimState m) a)
{-# inline cloneMutableUnliftedArray #-}
cloneMutableUnliftedArray src off len = do
    dst <- unsafeNewUnliftedArray len
    copyMutableUnliftedArray dst 0 src off len
    return dst
