{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Z.Data.Utils.Unaligned
Description : unaligned access for primitive arrays
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module implements unaligned element access with ghc primitives (> 8.6), which can be used
as a simple binary encoding \/ decoding method.
-}

module Z.Data.Utils.Unaligned where

import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Primitive.Types
import           GHC.Int
import           GHC.ST
import           GHC.Exts
import           GHC.Word
import           GHC.Float      (stgFloatToWord32, stgWord32ToFloat, stgWord64ToDouble, stgDoubleToWord64)
import           Foreign.C.Types

#include "MachDeps.h"

--------------------------------------------------------------------------------

newtype UnalignedSize a 
    = UnalignedSize 
    { getUnalignedSize :: Int
    }   deriving (Show, Eq, Ord)
        deriving newtype Num

-- | Primitive types which can be unaligned accessed
--
-- It can also be used as a lightweight method to peek\/poke value from\/to C structs
-- when you pass 'MutableByteArray#' to FFI as struct pointer, e.g.
--
-- @
--  -- | note the .hsc syntax
--  peekSocketAddrMBA :: HasCallStack => MBA## SocketAddr -> IO SocketAddr
--  peekSocketAddrMBA p = do
--      family <- peekMBA p (#offset struct sockaddr, sa_family)
--      case family :: CSaFamily of
--          (#const AF_INET) -> do
--              addr <- peekMBA p (#offset struct sockaddr_in, sin_addr)
--              port <- peekMBA p (#offset struct sockaddr_in, sin_port)
--              return (SocketAddrInet (PortNumber port) addr)
--          ....
-- @
--
class Unaligned a where
    {-# MINIMAL unalignedSize,
                (indexOff# | indexOff),
                (writeOff# | writeOff),
                (readOff#  | readOff) #-}
    -- | byte size
    unalignedSize :: UnalignedSize a

    -- | index element off byte array with offset in bytes(maybe unaligned)
    indexOff# :: ByteArray# -> Int# -> a
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = indexOff (PrimArray ba#) (I# i#)

    -- | read element from byte array with offset in bytes(maybe unaligned)
    readOff#  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    {-# INLINE readOff# #-}
    readOff# mba# i# s# =
        case readOff (MutablePrimArray mba#) (I# i#) of
            ST st -> st s#

    -- | write element to byte array with offset in bytes(maybe unaligned)
    writeOff# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
    {-# INLINE  writeOff# #-}
    writeOff# mba# i# x s# =
        case writeOff (MutablePrimArray mba#) (I# i#) x of
            (ST st) -> case st s# of (# s'#, _ #) -> s'#

    -- | Lifted version of 'writeOff#'
    writeOff :: Unaligned a => MutablePrimArray s Word8 -> Int -> a -> ST s ()
    {-# INLINE writeOff #-}
    writeOff (MutablePrimArray mba#) (I# i#) x =
        ST (\ s# -> (# writeOff# mba# i# x s#, () #))

    -- | Lifted version of 'readOff#'
    readOff :: MutablePrimArray s Word8 -> Int -> ST s a
    {-# INLINE readOff #-}
    readOff (MutablePrimArray mba#) (I# i#) = ST (readOff# mba# i#)

    -- | Lifted version of 'indexOff#'
    indexOff :: Unaligned a => PrimArray Word8 -> Int -> a
    {-# INLINE indexOff #-}
    indexOff (PrimArray ba#) (I# i#) = indexOff# ba# i#

-- | Encode PrimArray elements to Word8 Array in big endian.
--
-- The index is given in elements.
toBEArray
    :: forall a. (Prim a, Unaligned (BE a)) 
    => PrimArray a 
    -> Int      -- ^ start offset
    -> Int      -- ^ length
    -> PrimArray Word8
{-# INLINE toBEArray #-}
toBEArray parr off len =
#if defined(WORDS_BIGENDIAN)
    unsafeCoerce# (clonePrimArray parr off len)
#else
    runST (do
        buf <- newPrimArray siz
        go buf off 0)
  where
    s = getUnalignedSize (unalignedSize @(BE a))
    siz = len * s
    go :: MutablePrimArray s Word8 -> Int -> Int -> ST s (PrimArray Word8)
    go buf !i !j
        | j == siz = unsafeFreezePrimArray buf
        | otherwise = do
            writeOff buf j (BE (indexPrimArray parr i))
            go buf (i+1) (j+s)
#endif

-- | Decode PrimArray elements from a Word8 Array in big endian.
fromBEArray :: forall a. (Prim a, Unaligned (BE a)) => PrimArray Word8 -> Int -> Int -> PrimArray a
{-# INLINE fromBEArray #-}
fromBEArray parr off len =
#if defined(WORDS_BIGENDIAN)
    unsafeCoerce# (clonePrimArray parr off len)
#else
    runST $ do
        buf <- newPrimArray siz
        go buf off 0
  where
    s = getUnalignedSize (unalignedSize @(BE a))
    siz = len `quot` s
    go :: MutablePrimArray s a -> Int -> Int -> ST s (PrimArray a)
    go buf !i !j
        | j == siz = unsafeFreezePrimArray buf
        | otherwise = do
            writePrimArray buf j (getBE (indexOff parr i))
            go buf (i+s) (j+1)
#endif

-- | Encode PrimArray elements to Word8 Array in big endian.
--
-- The index is given in elements.
toLEArray
    :: forall a. (Prim a, Unaligned (LE a)) 
    => PrimArray a 
    -> Int      -- ^ start offset
    -> Int      -- ^ length
    -> PrimArray Word8
{-# INLINE toLEArray #-}
toLEArray parr off len =
#if defined(WORDS_BIGENDIAN)
    runST $ do
        buf <- newPrimArray siz
        go buf off 0
  where
    s = getUnalignedSize (unalignedSize @(LE a))
    siz = len * s
    go :: MutablePrimArray s Word8 -> Int -> Int -> ST s (PrimArray Word8)
    go buf !i !j
        | j == siz = unsafeFreezePrimArray buf
        | otherwise = do
            writeOff buf j (LE (indexPrimArray parr i))
            go buf (i+1) (j+s)
#else
    unsafeCoerce# (clonePrimArray parr off len)
#endif

-- | Decode PrimArray elements from a Word8 Array in big endian.
fromLEArray :: forall a. (Prim a, Unaligned (LE a)) => PrimArray Word8 -> Int -> Int -> PrimArray a
{-# INLINE fromLEArray #-}
fromLEArray parr off len =
#if defined(WORDS_BIGENDIAN)
    runST $ do
        buf <- newPrimArray siz
        go buf off 0
  where
    s = getUnalignedSize (unalignedSize @(LE a))
    siz = len `quot` s
    go :: MutablePrimArray s a -> Int -> Int -> ST s (PrimArray a)
    go buf !i !j
        | j == siz = unsafeFreezePrimArray buf
        | otherwise = do
            writePrimArray buf j (getLE (indexOff parr i))
            go buf (i+s) (j+1)
#else
    unsafeCoerce# (clonePrimArray parr off len)
#endif

instance Unaligned Word8 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 1
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (W8# x#) = writeWord8Array# mba# i# x#
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8Array# mba# i# s0 in (# s1, W8# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = W8# (indexWord8Array# ba# i#)

instance Unaligned Int8 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 1
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (I8# x#) = writeInt8Array# mba# i# x#
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readInt8Array# mba# i# s0 in (# s1, I8# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = I8# (indexInt8Array# ba# i#)

-- | little endianess wrapper
--
newtype LE a = LE { getLE :: a } deriving (Show, Eq)

-- | big endianess wrapper
--
newtype BE a = BE { getBE :: a } deriving (Show, Eq)

#define USE_HOST_IMPL(END) \
    {-# INLINE writeOff# #-}; \
    writeOff# mba# i# (END x) = writeOff# mba# i# x; \
    {-# INLINE readOff# #-}; \
    readOff# mba# i# s0 = \
        let !(# s1, x #) = readOff# mba# i# s0 in (# s1, END x #); \
    {-# INLINE indexOff# #-}; \
    indexOff# ba# i# = END (indexOff# ba# i#);

--------------------------------------------------------------------------------

instance Unaligned Word16 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (W16# x#) = writeWord8ArrayAsWord16# mba# i# x#
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord16# mba# i# s0 in (# s1, W16# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = W16# (indexWord8ArrayAsWord16# ba# i#)

word16ToWord8# :: Word16# -> Word8#
{-# INLINE word16ToWord8# #-}
word16ToWord8# w# = wordToWord8# (word16ToWord# w#)

word8ToWord16# :: Word8# -> Word16#
{-# INLINE word8ToWord16# #-}
word8ToWord16# w# = wordToWord16# (word8ToWord# w#)

instance Unaligned (LE Word16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE w) s0 = writeOff# mba# i# (byteSwap16 w) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord16# mba# i# s0
        in (# s1, LE (byteSwap16 (W16# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = LE (byteSwap16 (W16# (indexWord8ArrayAsWord16# ba# i#)))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Word16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE w) s0 = writeOff# mba# i# (byteSwap16 w) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord16# mba# i# s0
        in (# s1, BE (byteSwap16 (W16# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = BE (byteSwap16 (W16# (indexWord8ArrayAsWord16# ba# i#)))
#endif

--------------------------------------------------------------------------------

instance Unaligned Word32 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (W32# x#) s0 = writeWord8ArrayAsWord32# mba# i# x# s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord32# mba# i# s0 in (# s1, W32# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = W32# (indexWord8ArrayAsWord32# ba# i#)


instance Unaligned (LE Word32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE w) s0 = writeOff# mba# i# (byteSwap32 w) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord32# mba# i# s0
        in (# s1, LE (byteSwap32 (W32# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = LE (byteSwap32 (W32# (indexWord8ArrayAsWord32# ba# i#)))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Word32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE x) s0 = writeOff# mba# i# (byteSwap32 x) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord32# mba# i# s0
        in (# s1, BE (byteSwap32 (W32# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = BE (byteSwap32 (W32# (indexWord8ArrayAsWord32# ba# i#)))
#endif

--------------------------------------------------------------------------------

instance Unaligned Word64 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (W64# x#) s0 = writeWord8ArrayAsWord64# mba# i# x# s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord64# mba# i# s0 in (# s1, W64# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = W64# (indexWord8ArrayAsWord64# ba# i#)


instance Unaligned (LE Word64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE w) s0 = writeOff# mba# i# (byteSwap64 w) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord64# mba# i# s0
        in (# s1, LE (byteSwap64 (W64# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = LE (byteSwap64 (W64# (indexWord8ArrayAsWord64# ba# i#)))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Word64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE x) s0 = writeOff# mba# i# (byteSwap64 x) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord64# mba# i# s0
        in (# s1, BE (byteSwap64 (W64# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = BE (byteSwap64 (W64# (indexWord8ArrayAsWord64# ba# i#)))
#endif

--------------------------------------------------------------------------------

instance Unaligned Word where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#endif
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (W# x#) s0 = writeWord8ArrayAsWord# mba# i# x# s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord# mba# i# s0 in (# s1, W# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = W# (indexWord8ArrayAsWord# ba# i#)

instance Unaligned (LE Word) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#endif
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE (W# x#)) s0 = writeWord8ArrayAsWord# mba# i# (byteSwap# x#) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord# mba# i# s0 in (# s1, LE (W# (byteSwap# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = LE (W# (byteSwap# (indexWord8ArrayAsWord# ba# i#)))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Word) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#endif
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE (W# x#)) s0 = writeWord8ArrayAsWord# mba# i# (byteSwap# x#) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord# mba# i# s0 in (# s1, BE (W# (byteSwap# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = BE (W# (byteSwap# (indexWord8ArrayAsWord# ba# i#)))
#endif

--------------------------------------------------------------------------------

instance Unaligned Int16 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (I16# x#) = writeWord8ArrayAsInt16# mba# i# x#
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt16# mba# i# s0 in (# s1, I16# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = I16# (indexWord8ArrayAsInt16# ba# i#)

int16ToWord8# :: Int16# -> Word8#
{-# INLINE int16ToWord8# #-}
int16ToWord8# w# = wordToWord8# (word16ToWord# (int16ToWord16# w#))

instance Unaligned (LE Int16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE (I16# x#)) s0 =
        let s1 = writeWord8Array# mba# i# (int16ToWord8# x#) s0
        in       writeWord8Array# mba# (i# +# 1#) (int16ToWord8# (uncheckedShiftRLInt16# x# 8#)) s1
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, w1# #) = readWord8Array# mba# i# s0
            !(# s2, w2# #) = readWord8Array# mba# (i# +# 1#) s1
        in (# s2, LE (I16# (word16ToInt16# (uncheckedShiftRLWord16# (word8ToWord16# w2#) 8# `orWord16#` (word8ToWord16# w1#)))) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# =
        let w1# = indexWord8Array# ba# i#
            w2# = indexWord8Array# ba# (i# +# 1#)
        in LE (I16# (word16ToInt16# (uncheckedShiftRLWord16# (word8ToWord16# w2#) 8# `orWord16#` (word8ToWord16# w1#))))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Int16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE (I16# x#)) s0 =
        let s1 = writeWord8Array# mba# i# (int16ToWord8# (uncheckedShiftRLInt16# x# 8#)) s0
        in       writeWord8Array# mba# (i# +# 1#) (int16ToWord8# x#) s1
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, w1# #) = readWord8Array# mba# i# s0
            !(# s2, w2# #) = readWord8Array# mba# (i# +# 1#) s1
        in (# s2, BE (I16# (word16ToInt16# (uncheckedShiftLWord16# (word8ToWord16# w1#) 8# `orWord16#` (word8ToWord16# w2#)))) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# =
        let w1# = indexWord8Array# ba# i#
            w2# = indexWord8Array# ba# (i# +# 1#)
        in BE (I16# (word16ToInt16# (uncheckedShiftLWord16# (word8ToWord16# w1#) 8# `orWord16#` (word8ToWord16# w2#))))
#endif

--------------------------------------------------------------------------------

instance Unaligned Int32 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (I32# x#) s0 = writeWord8ArrayAsInt32# mba# i# x# s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt32# mba# i# s0 in (# s1, I32# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = I32# (indexWord8ArrayAsInt32# ba# i#)

byteSwapInt32 :: Int32 -> Int32
{-# INLINE byteSwapInt32 #-}
byteSwapInt32 i = fromIntegral (byteSwap32 (fromIntegral i))

instance Unaligned (LE Int32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE w) s0 = writeOff# mba# i# (byteSwapInt32 w) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord32# mba# i# s0
        in (# s1, LE (byteSwapInt32 (I32# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = LE (byteSwapInt32 (I32# (indexWord8ArrayAsInt32# ba# i#)))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Int32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE x) s0 = writeOff# mba# i# (byteSwapInt32 x) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt32# mba# i# s0
        in (# s1, BE (byteSwapInt32 (I32# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = BE (byteSwapInt32 (I32# (indexWord8ArrayAsInt32# ba# i#)))
#endif

--------------------------------------------------------------------------------

instance Unaligned Int64 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (I64# x#) s0 = writeWord8ArrayAsInt64# mba# i# x# s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt64# mba# i# s0 in (# s1, I64# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = I64# (indexWord8ArrayAsInt64# ba# i#)

byteSwapInt64 :: Int64 -> Int64
{-# INLINE byteSwapInt64 #-}
byteSwapInt64 i = fromIntegral (byteSwap64 (fromIntegral i))

instance Unaligned (LE Int64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE w) s0 = writeOff# mba# i# (byteSwapInt64 w) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt64# mba# i# s0
        in (# s1, LE (byteSwapInt64 (I64# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = LE (byteSwapInt64 (I64# (indexWord8ArrayAsInt64# ba# i#)))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Int64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE x) s0 = writeOff# mba# i# (byteSwapInt64 x) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt64# mba# i# s0
        in (# s1, BE (byteSwapInt64 (I64# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = BE (byteSwapInt64 (I64# (indexWord8ArrayAsInt64# ba# i#)))
#endif

--------------------------------------------------------------------------------

instance Unaligned Int where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#endif
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (I# x#) s0 = writeWord8ArrayAsInt# mba# i# x# s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt# mba# i# s0 in (# s1, I# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = I# (indexWord8ArrayAsInt# ba# i#)

byteSwapInt :: Int -> Int
{-# INLINE byteSwapInt #-}
byteSwapInt (I# i#) = I# (word2Int# (byteSwap# (int2Word# i#)))

instance Unaligned (LE Int) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#endif
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE x) s0 = writeOff# mba# i# (byteSwapInt x) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x #) = readOff# mba# i# s0 in (# s1, LE (byteSwapInt x) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = LE (byteSwapInt (indexOff# ba# i#))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Int) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#endif
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE x) s0 = writeOff# mba# i# (byteSwapInt x) s0
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x #) = readOff# mba# i# s0 in (# s1, BE (byteSwapInt x) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = BE (byteSwapInt (indexOff# ba# i#))
#endif

--------------------------------------------------------------------------------

instance Unaligned (Ptr a) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize SIZEOF_HSPTR
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (Ptr x#) = writeWord8ArrayAsAddr# mba# i# x#
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsAddr# mba# i# s0 in (# s1, Ptr x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = Ptr (indexWord8ArrayAsAddr# ba# i#)

instance Unaligned Float where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (F# x#) = writeWord8ArrayAsFloat# mba# i# x#
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsFloat# mba# i# s0 in (# s1, F# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = F# (indexWord8ArrayAsFloat# ba# i#)

instance Unaligned (LE Float) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE (F# x#)) =
        writeOff# mba# i# (LE (W32# (stgFloatToWord32 x#)))
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, LE (W32# x#) #) = readOff# mba# i# s0
        in (# s1, LE (F# (stgWord32ToFloat x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# =
        let LE (W32# x#) = indexOff# ba# i#
        in LE (F# (stgWord32ToFloat x#))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Float) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE (F# x#)) =
        writeOff# mba# i# (BE (W32# (stgFloatToWord32 x#)))
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, BE (W32# x#) #) = readOff# mba# i# s0
        in (# s1, BE (F# (stgWord32ToFloat x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# =
        let !(BE (W32# x#)) = indexOff# ba# i#
        in BE (F# (stgWord32ToFloat x#))
#endif

--------------------------------------------------------------------------------

instance Unaligned Double where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (D# x#) = writeWord8ArrayAsDouble# mba# i# x#
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsDouble# mba# i# s0 in (# s1, D# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = D# (indexWord8ArrayAsDouble# ba# i#)

instance Unaligned (LE Double) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE (D# x#)) =
        writeOff# mba# i# (LE (W64# (stgDoubleToWord64 x#)))
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, LE (W64# x#) #) = readOff# mba# i# s0
        in (# s1, LE (D# (stgWord64ToDouble x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# =
        let LE (W64# x#) = indexOff# ba# i#
        in LE (D# (stgWord64ToDouble x#))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Double) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE (D# x#)) =
        writeOff# mba# i# (BE (W64# (stgDoubleToWord64 x#)))
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, BE (W64# x#) #) = readOff# mba# i# s0
        in (# s1, BE (D# (stgWord64ToDouble x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# =
        let !(BE (W64# x#)) = indexOff# ba# i#
        in BE (D# (stgWord64ToDouble x#))
#endif

--------------------------------------------------------------------------------

-- | Char's instance use 31bit wide char prim-op.
instance Unaligned Char where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (C# x#) = writeWord8ArrayAsWideChar# mba# i# x#
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWideChar# mba# i# s0 in (# s1, C# x# #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# = C# (indexWord8ArrayAsWideChar# ba# i#)

instance Unaligned (LE Char) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN)
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (LE (C# x#)) =
        writeOff# mba# i# (LE (I32# (ord# x#)))
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, LE (I32# x#) #) = readOff# mba# i# s0
        in (# s1, LE (C# (chr# x#)) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# =
        let LE (I32# x#) = indexOff# ba# i#
        in LE (C# (chr# x#))
#else
    USE_HOST_IMPL(LE)
#endif

instance Unaligned (BE Char) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeOff# #-}
    writeOff# mba# i# (BE (C# x#)) =
        writeOff# mba# i# (BE (I32# (intToInt32# (ord# x#))))
    {-# INLINE readOff# #-}
    readOff# mba# i# s0 =
        let !(# s1, BE (I32# x#) #) = readOff# mba# i# s0
        in (# s1, BE (C# (chr# (int32ToInt# x#))) #)
    {-# INLINE indexOff# #-}
    indexOff# ba# i# =
        let !(BE (I32# x#)) = indexOff# ba# i#
        in BE (C# (chr# (int32ToInt# x#)))
#endif

-- | Write a, b in order
instance (Unaligned a, Unaligned b) => Unaligned (a, b) where
    {-# INLINE unalignedSize #-}
    unalignedSize =
        UnalignedSize ( getUnalignedSize (unalignedSize @a)
                      + getUnalignedSize (unalignedSize @b))
    {-# INLINE writeOff #-}
    writeOff mba i (a, b) = do
        writeOff mba i a
        writeOff mba j b
      where
        j = i + getUnalignedSize (unalignedSize @a)

    {-# INLINE readOff #-}
    readOff mba i = do
        !a <- readOff mba i
        !b <- readOff mba j
        return (a, b)
      where
        j = i + getUnalignedSize (unalignedSize @a)

    {-# INLINE indexOff #-}
    indexOff ba i =
        let !a = indexOff ba i
            !b = indexOff ba j
        in (a, b)
      where
        j = i + getUnalignedSize (unalignedSize @a)

-- | Write a, b, c in order
instance (Unaligned a, Unaligned b, Unaligned c) => Unaligned (a, b, c) where
    {-# INLINE unalignedSize #-}
    unalignedSize =
        UnalignedSize ( getUnalignedSize (unalignedSize @a)
                      + getUnalignedSize (unalignedSize @b)
                      + getUnalignedSize (unalignedSize @c))
    {-# INLINE writeOff #-}
    writeOff mba i (a, b, c) = do
        writeOff mba i a
        writeOff mba j b
        writeOff mba k c
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)

    {-# INLINE readOff #-}
    readOff mba i = do
        !a <- readOff mba i
        !b <- readOff mba j
        !c <- readOff mba k
        return (a, b, c)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)

    {-# INLINE indexOff #-}
    indexOff ba i =
        let !a = indexOff ba i
            !b = indexOff ba j
            !c = indexOff ba k
        in (a, b, c)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)

-- | Write a, b, c, d in order
instance (Unaligned a, Unaligned b, Unaligned c, Unaligned d) => Unaligned (a, b, c, d) where
    {-# INLINE unalignedSize #-}
    unalignedSize =
        UnalignedSize ( getUnalignedSize (unalignedSize @a)
                      + getUnalignedSize (unalignedSize @b)
                      + getUnalignedSize (unalignedSize @c)
                      + getUnalignedSize (unalignedSize @d))
    {-# INLINE writeOff #-}
    writeOff mba i (a, b, c, d) = do
        writeOff mba i a
        writeOff mba j b
        writeOff mba k c
        writeOff mba l d
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)

    {-# INLINE readOff #-}
    readOff mba i = do
        !a <- readOff mba i
        !b <- readOff mba j
        !c <- readOff mba k
        !d <- readOff mba l
        return (a, b, c, d)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)

    {-# INLINE indexOff #-}
    indexOff ba i =
        let !a = indexOff ba i
            !b = indexOff ba j
            !c = indexOff ba k
            !d = indexOff ba l
        in (a, b, c, d)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)

-- | Write a, b, c, d, e in order
instance (Unaligned a, Unaligned b, Unaligned c, Unaligned d, Unaligned e) => Unaligned (a, b, c, d, e) where
    {-# INLINE unalignedSize #-}
    unalignedSize =
        UnalignedSize ( getUnalignedSize (unalignedSize @a)
                      + getUnalignedSize (unalignedSize @b)
                      + getUnalignedSize (unalignedSize @c)
                      + getUnalignedSize (unalignedSize @d)
                      + getUnalignedSize (unalignedSize @e))
    {-# INLINE writeOff #-}
    writeOff mba i (a, b, c, d, e) = do
        writeOff mba i a
        writeOff mba j b
        writeOff mba k c
        writeOff mba l d
        writeOff mba m e
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)

    {-# INLINE readOff #-}
    readOff mba i = do
        !a <- readOff mba i
        !b <- readOff mba j
        !c <- readOff mba k
        !d <- readOff mba l
        !e <- readOff mba m
        return (a, b, c, d, e)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)

    {-# INLINE indexOff #-}
    indexOff ba i =
        let !a = indexOff ba i
            !b = indexOff ba j
            !c = indexOff ba k
            !d = indexOff ba l
            !e = indexOff ba m
        in (a, b, c, d, e)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)

-- | Write a, b, c, d, e, f in order
instance (Unaligned a, Unaligned b, Unaligned c, Unaligned d, Unaligned e, Unaligned f)
    => Unaligned (a, b, c, d, e, f)
  where
    {-# INLINE unalignedSize #-}
    unalignedSize =
        UnalignedSize ( getUnalignedSize (unalignedSize @a)
                      + getUnalignedSize (unalignedSize @b)
                      + getUnalignedSize (unalignedSize @c)
                      + getUnalignedSize (unalignedSize @d)
                      + getUnalignedSize (unalignedSize @e)
                      + getUnalignedSize (unalignedSize @f))
    {-# INLINE writeOff #-}
    writeOff mba i (a, b, c, d, e, f) = do
        writeOff mba i a
        writeOff mba j b
        writeOff mba k c
        writeOff mba l d
        writeOff mba m e
        writeOff mba n f
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)

    {-# INLINE readOff #-}
    readOff mba i = do
        !a <- readOff mba i
        !b <- readOff mba j
        !c <- readOff mba k
        !d <- readOff mba l
        !e <- readOff mba m
        !f <- readOff mba n
        return (a, b, c, d, e, f)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)

    {-# INLINE indexOff #-}
    indexOff ba i =
        let !a = indexOff ba i
            !b = indexOff ba j
            !c = indexOff ba k
            !d = indexOff ba l
            !e = indexOff ba m
            !f = indexOff ba n
        in (a, b, c, d, e, f)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)

-- | Write a, b, c, d, e, f, g in order
instance (Unaligned a, Unaligned b, Unaligned c, Unaligned d, Unaligned e, Unaligned f, Unaligned g)
    => Unaligned (a, b, c, d, e, f, g)
  where
    {-# INLINE unalignedSize #-}
    unalignedSize =
        UnalignedSize ( getUnalignedSize (unalignedSize @a)
                      + getUnalignedSize (unalignedSize @b)
                      + getUnalignedSize (unalignedSize @c)
                      + getUnalignedSize (unalignedSize @d)
                      + getUnalignedSize (unalignedSize @e)
                      + getUnalignedSize (unalignedSize @f)
                      + getUnalignedSize (unalignedSize @g))
    {-# INLINE writeOff #-}
    writeOff mba i (a, b, c, d, e, f, g) = do
        writeOff mba i a
        writeOff mba j b
        writeOff mba k c
        writeOff mba l d
        writeOff mba m e
        writeOff mba n f
        writeOff mba o g
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)
        o = n + getUnalignedSize (unalignedSize @f)

    {-# INLINE readOff #-}
    readOff mba i = do
        !a <- readOff mba i
        !b <- readOff mba j
        !c <- readOff mba k
        !d <- readOff mba l
        !e <- readOff mba m
        !f <- readOff mba n
        !g <- readOff mba o
        return (a, b, c, d, e, f, g)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)
        o = n + getUnalignedSize (unalignedSize @f)

    {-# INLINE indexOff #-}
    indexOff ba i =
        let !a = indexOff ba i
            !b = indexOff ba j
            !c = indexOff ba k
            !d = indexOff ba l
            !e = indexOff ba m
            !f = indexOff ba n
            !g = indexOff ba o
        in (a, b, c, d, e, f, g)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)
        o = n + getUnalignedSize (unalignedSize @f)

-- | Write a, b, c, d, e, f, g, h in order
instance (Unaligned a, Unaligned b, Unaligned c, Unaligned d, Unaligned e, Unaligned f, Unaligned g, Unaligned h)
    => Unaligned (a, b, c, d, e, f, g, h)
  where
    {-# INLINE unalignedSize #-}
    unalignedSize =
        UnalignedSize ( getUnalignedSize (unalignedSize @a)
                      + getUnalignedSize (unalignedSize @b)
                      + getUnalignedSize (unalignedSize @c)
                      + getUnalignedSize (unalignedSize @d)
                      + getUnalignedSize (unalignedSize @e)
                      + getUnalignedSize (unalignedSize @f)
                      + getUnalignedSize (unalignedSize @g)
                      + getUnalignedSize (unalignedSize @h))
    {-# INLINE writeOff #-}
    writeOff mba i (a, b, c, d, e, f, g, h) = do
        writeOff mba i a
        writeOff mba j b
        writeOff mba k c
        writeOff mba l d
        writeOff mba m e
        writeOff mba n f
        writeOff mba o g
        writeOff mba p h
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)
        o = n + getUnalignedSize (unalignedSize @f)
        p = o + getUnalignedSize (unalignedSize @g)

    {-# INLINE readOff #-}
    readOff mba i = do
        !a <- readOff mba i
        !b <- readOff mba j
        !c <- readOff mba k
        !d <- readOff mba l
        !e <- readOff mba m
        !f <- readOff mba n
        !g <- readOff mba o
        !h <- readOff mba p
        return (a, b, c, d, e, f, g, h)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)
        o = n + getUnalignedSize (unalignedSize @f)
        p = o + getUnalignedSize (unalignedSize @g)

    {-# INLINE indexOff #-}
    indexOff ba i =
        let !a = indexOff ba i
            !b = indexOff ba j
            !c = indexOff ba k
            !d = indexOff ba l
            !e = indexOff ba m
            !f = indexOff ba n
            !g = indexOff ba o
            !h = indexOff ba p
        in (a, b, c, d, e, f, g, h)
      where
        j = i + getUnalignedSize (unalignedSize @a)
        k = j + getUnalignedSize (unalignedSize @b)
        l = k + getUnalignedSize (unalignedSize @c)
        m = l + getUnalignedSize (unalignedSize @d)
        n = m + getUnalignedSize (unalignedSize @e)
        o = n + getUnalignedSize (unalignedSize @f)
        p = o + getUnalignedSize (unalignedSize @g)

--------------------------------------------------------------------------------

-- Prim instances for newtypes in Foreign.C.Types
deriving newtype instance Unaligned CChar
deriving newtype instance Unaligned CSChar
deriving newtype instance Unaligned CUChar
deriving newtype instance Unaligned CShort
deriving newtype instance Unaligned CUShort
deriving newtype instance Unaligned CInt
deriving newtype instance Unaligned CUInt
deriving newtype instance Unaligned CLong
deriving newtype instance Unaligned CULong
deriving newtype instance Unaligned CPtrdiff
deriving newtype instance Unaligned CSize
deriving newtype instance Unaligned CWchar
deriving newtype instance Unaligned CSigAtomic
deriving newtype instance Unaligned CLLong
deriving newtype instance Unaligned CULLong
deriving newtype instance Unaligned CBool
deriving newtype instance Unaligned CIntPtr
deriving newtype instance Unaligned CUIntPtr
deriving newtype instance Unaligned CIntMax
deriving newtype instance Unaligned CUIntMax
deriving newtype instance Unaligned CClock
deriving newtype instance Unaligned CTime
deriving newtype instance Unaligned CUSeconds
deriving newtype instance Unaligned CSUSeconds
deriving newtype instance Unaligned CFloat
deriving newtype instance Unaligned CDouble
