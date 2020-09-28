{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

{-|
Module      : Z.Data.Array.UnalignedAccess
Description : unaligned access for primitive arrays
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module implements unaligned element access with ghc primitives (> 8.6).
-}

module Z.Data.Array.UnalignedAccess where

import           Control.Monad.Primitive
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           GHC.Int
import           GHC.Exts
import           GHC.Word
import           GHC.Float (stgFloatToWord32, stgWord32ToFloat, stgWord64ToDouble, stgDoubleToWord64)
import           Foreign.C.Types

-- toggle these defs to test different implements
#define USE_BSWAP
-- #define USE_SHIFT

--------------------------------------------------------------------------------

newtype UnalignedSize a = UnalignedSize { getUnalignedSize :: Int } deriving (Show, Eq)

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
class UnalignedAccess a where
    {-# MINIMAL unalignedSize, indexWord8ArrayAs#, writeWord8ArrayAs#, readWord8ArrayAs# |
        unalignedSize, indexBA, peekMBA, pokeMBA #-}
    -- | byte size
    unalignedSize :: UnalignedSize a

    -- | index element off byte array with offset in bytes(maybe unaligned)
    indexWord8ArrayAs# :: ByteArray# -> Int# -> a
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = indexBA ba# (I# i#)

    -- | read element from byte array with offset in bytes(maybe unaligned)
    readWord8ArrayAs#  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    {-# INLINE  readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s# =
        (unsafeCoerce# (peekMBA (unsafeCoerce# mba#) (I# i#) :: IO a)) s#

    -- | write element to byte array with offset in bytes(maybe unaligned)
    writeWord8ArrayAs# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
    {-# INLINE  writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# x s# =
        unsafeCoerce# (pokeMBA (unsafeCoerce# mba#) (I# i#) x) s#

    -- | IO version of 'writeWord8ArrayAs#' but more convenient to write manually.
    peekMBA :: MutableByteArray# RealWorld -> Int -> IO a
    {-# INLINE peekMBA #-}
    peekMBA mba# (I# i#) = primitive (readWord8ArrayAs# mba# i#)

    -- | IO version of 'readWord8ArrayAs#' but more convenient to write manually.
    pokeMBA  :: MutableByteArray# RealWorld -> Int -> a -> IO ()
    {-# INLINE pokeMBA #-}
    pokeMBA mba# (I# i#) x = primitive_ (writeWord8ArrayAs# mba# i# x)

    -- | index element off byte array with offset in bytes(maybe unaligned)
    indexBA :: ByteArray# -> Int -> a
    {-# INLINE indexBA #-}
    indexBA ba# (I# i#) = indexWord8ArrayAs# ba# i#


-- | Lifted version of 'writeWord8ArrayAs#'
writeWord8ArrayAs :: (PrimMonad m, UnalignedAccess a) => MutableByteArray (PrimState m) -> Int -> a -> m ()
{-# INLINE writeWord8ArrayAs #-}
writeWord8ArrayAs (MutableByteArray mba#) (I# i#) x = primitive_ (writeWord8ArrayAs# mba# i# x)

-- | Lifted version of 'readWord8ArrayAs#'
readWord8ArrayAs :: (PrimMonad m, UnalignedAccess a) => MutableByteArray (PrimState m) -> Int -> m a
{-# INLINE readWord8ArrayAs #-}
readWord8ArrayAs (MutableByteArray mba#) (I# i#) = primitive (readWord8ArrayAs# mba# i#)

-- | Lifted version of 'indexWord8ArrayAs#'
indexWord8ArrayAs :: UnalignedAccess a => ByteArray -> Int -> a
{-# INLINE indexWord8ArrayAs #-}
indexWord8ArrayAs (ByteArray ba#) (I# i#) = indexWord8ArrayAs# ba# i#

-- | Lifted version of 'writeWord8ArrayAs#'
writePrimWord8ArrayAs :: (PrimMonad m, UnalignedAccess a) => MutablePrimArray (PrimState m) Word8 -> Int -> a -> m ()
{-# INLINE writePrimWord8ArrayAs #-}
writePrimWord8ArrayAs (MutablePrimArray mba#) (I# i#) x = primitive_ (writeWord8ArrayAs# mba# i# x)

-- | Lifted version of 'readWord8ArrayAs#'
readPrimWord8ArrayAs :: (PrimMonad m, UnalignedAccess a) => MutablePrimArray (PrimState m) Word8 -> Int -> m a
{-# INLINE readPrimWord8ArrayAs #-}
readPrimWord8ArrayAs (MutablePrimArray mba#) (I# i#) = primitive (readWord8ArrayAs# mba# i#)

-- | Lifted version of 'indexWord8ArrayAs#'
indexPrimWord8ArrayAs :: UnalignedAccess a => PrimArray Word8 -> Int -> a
{-# INLINE indexPrimWord8ArrayAs #-}
indexPrimWord8ArrayAs (PrimArray ba#) (I# i#) = indexWord8ArrayAs# ba# i#

instance UnalignedAccess Word8 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 1
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (W8# x#) = writeWord8Array# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8Array# mba# i# s0 in (# s1, W8# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = W8# (indexWord8Array# ba# i#)

instance UnalignedAccess Int8 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 1
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (I8# x#) = writeInt8Array# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readInt8Array# mba# i# s0 in (# s1, I8# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = I8# (indexInt8Array# ba# i#)

-- | little endianess wrapper
--
newtype LE a = LE { getLE :: a } deriving (Show, Eq)

-- | big endianess wrapper
--
newtype BE a = BE { getBE :: a } deriving (Show, Eq)

#define USE_HOST_IMPL(END) \
    {-# INLINE writeWord8ArrayAs# #-}; \
    writeWord8ArrayAs# mba# i# (END x) = writeWord8ArrayAs# mba# i# x; \
    {-# INLINE readWord8ArrayAs# #-}; \
    readWord8ArrayAs# mba# i# s0 = \
        let !(# s1, x #) = readWord8ArrayAs# mba# i# s0 in (# s1, END x #); \
    {-# INLINE indexWord8ArrayAs# #-}; \
    indexWord8ArrayAs# ba# i# = END (indexWord8ArrayAs# ba# i#);

--------------------------------------------------------------------------------

instance UnalignedAccess Word16 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (W16# x#) = writeWord8ArrayAsWord16# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord16# mba# i# s0 in (# s1, W16# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = W16# (indexWord8ArrayAsWord16# ba# i#)

instance UnalignedAccess (LE Word16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (W16# x#)) s0# =
        let s1# = writeWord8Array# mba# i# x# s0#
        in        writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, w1# #) = readWord8Array# mba# i# s0
            !(# s2, w2# #) = readWord8Array# mba# (i# +# 1#) s1
        in (# s2, LE (W16# (uncheckedShiftL# w2# 8# `or#` w1#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let w1# = indexWord8Array# ba# i#
            w2# = indexWord8Array# ba# (i# +# 1#)
        in LE (W16# (uncheckedShiftL# w2# 8# `or#` w1#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Word16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
-- on X86 we use bswap
-- TODO: find out if arch64 support this
#if (defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)) && defined(USE_BSWAP)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (W16# x#)) = writeWord8ArrayAsWord16# mba# i# (byteSwap16# x#)
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord16# mba# i# s0
        in (# s1, BE (W16# (byteSwap16# x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = BE (W16# (byteSwap16# (indexWord8ArrayAsWord16# ba# i#)))
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (W16# x#)) s0# =
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 8#) s0#
        in        writeWord8Array# mba# (i# +# 1#) x# s1#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, w2# #) = readWord8Array# mba# i# s0
            !(# s2, w1# #) = readWord8Array# mba# (i# +# 1#) s1
        in (# s2, BE (W16# (uncheckedShiftL# w2# 8# `or#`  w1#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let w2# = indexWord8Array# ba# i#
            w1# = indexWord8Array# ba# (i# +# 1#)
        in BE (W16# (uncheckedShiftL# w2# 8# `or#`  w1#))
#endif
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Word32 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (W32# x#) =  writeWord8ArrayAsWord32# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord32# mba# i# s0 in (# s1, W32# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = W32# (indexWord8ArrayAsWord32# ba# i#)


instance UnalignedAccess (LE Word32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (W32# x#)) s0# =
        let s1# = writeWord8Array# mba# i# x# s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 16#) s2#
        in        writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 24#) s3#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, w1# #) = readWord8Array# mba# i# s0
            !(# s2, w2# #) = readWord8Array# mba# (i# +# 1#) s1
            !(# s3, w3# #) = readWord8Array# mba# (i# +# 2#) s2
            !(# s4, w4# #) = readWord8Array# mba# (i# +# 3#) s3
        in (# s4, LE (W32# ((uncheckedShiftL# w4# 24#) `or#`
                    (uncheckedShiftL# w3# 16#) `or#`
                        (uncheckedShiftL# w2# 8#) `or#` w1#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let w1# = indexWord8Array# ba# i#
            w2# = indexWord8Array# ba# (i# +# 1#)
            w3# = indexWord8Array# ba# (i# +# 2#)
            w4# = indexWord8Array# ba# (i# +# 3#)
        in LE (W32# ((uncheckedShiftL# w4# 24#) `or#`
                    (uncheckedShiftL# w3# 16#) `or#`
                        (uncheckedShiftL# w2# 8#) `or#` w1#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Word32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
-- on X86 we use bswap
-- TODO: find out if arch64 support this
#if (defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)) && defined(USE_BSWAP)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (W32# x#)) = writeWord8ArrayAsWord32# mba# i# (byteSwap32# x#)
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord32# mba# i# s0
        in (# s1, BE (W32# (byteSwap32# x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = BE (W32# (byteSwap32# (indexWord8ArrayAsWord32# ba# i#)))
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (W32# x#)) s0# =
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 24#) s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 16#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 8#) s2#
        in        writeWord8Array# mba# (i# +# 3#) x# s3#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, w4# #) = readWord8Array# mba# i# s0
            !(# s2, w3# #) = readWord8Array# mba# (i# +# 1#) s1
            !(# s3, w2# #) = readWord8Array# mba# (i# +# 2#) s2
            !(# s4, w1# #) = readWord8Array# mba# (i# +# 3#) s3
        in (# s4, BE (W32# ((uncheckedShiftL# w4# 24#) `or#`
                    (uncheckedShiftL# w3# 16#) `or#`
                        (uncheckedShiftL# w2# 8#) `or#` w1#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let w4# = indexWord8Array# ba# i#
            w3# = indexWord8Array# ba# (i# +# 1#)
            w2# = indexWord8Array# ba# (i# +# 2#)
            w1# = indexWord8Array# ba# (i# +# 3#)
        in BE (W32# ((uncheckedShiftL# w4# 24#) `or#`
                    (uncheckedShiftL# w3# 16#) `or#`
                        (uncheckedShiftL# w2# 8#) `or#` w1#))
#endif
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Word64 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (W64# x#) =  writeWord8ArrayAsWord64# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord64# mba# i# s0 in (# s1, W64# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = W64# (indexWord8ArrayAsWord64# ba# i#)


instance UnalignedAccess (LE Word64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (W64# x#)) s0# =
        let s1# = writeWord8Array# mba# i# x# s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 8#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 16#) s2#
            s4# = writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 24#) s3#
            s5# = writeWord8Array# mba# (i# +# 4#) (uncheckedShiftRL# x# 32#) s4#
            s6# = writeWord8Array# mba# (i# +# 5#) (uncheckedShiftRL# x# 40#) s5#
            s7# = writeWord8Array# mba# (i# +# 6#) (uncheckedShiftRL# x# 48#) s6#
        in        writeWord8Array# mba# (i# +# 7#) (uncheckedShiftRL# x# 56#) s7#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, w1# #) = readWord8Array# mba# i# s0
            !(# s2, w2# #) = readWord8Array# mba# (i# +# 1#) s1
            !(# s3, w3# #) = readWord8Array# mba# (i# +# 2#) s2
            !(# s4, w4# #) = readWord8Array# mba# (i# +# 3#) s3
            !(# s5, w5# #) = readWord8Array# mba# (i# +# 4#) s4
            !(# s6, w6# #) = readWord8Array# mba# (i# +# 5#) s5
            !(# s7, w7# #) = readWord8Array# mba# (i# +# 6#) s6
            !(# s8, w8# #) = readWord8Array# mba# (i# +# 7#) s7
        in (# s8, LE (W64# ((uncheckedShiftL# w8# 56#) `or#`
                    (uncheckedShiftL# w7# 48#) `or#`
                        (uncheckedShiftL# w6# 40#) `or#`
                            (uncheckedShiftL# w5# 32#) `or#`
                                (uncheckedShiftL# w4# 24#) `or#`
                                    (uncheckedShiftL# w3# 16#) `or#`
                                        (uncheckedShiftL# w2# 8#) `or#` w1#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let w1# = indexWord8Array# ba# i#
            w2# = indexWord8Array# ba# (i# +# 1#)
            w3# = indexWord8Array# ba# (i# +# 2#)
            w4# = indexWord8Array# ba# (i# +# 3#)
            w5# = indexWord8Array# ba# (i# +# 4#)
            w6# = indexWord8Array# ba# (i# +# 5#)
            w7# = indexWord8Array# ba# (i# +# 6#)
            w8# = indexWord8Array# ba# (i# +# 7#)
        in LE (W64# ((uncheckedShiftL# w8# 56#) `or#`
                    (uncheckedShiftL# w7# 48#) `or#`
                        (uncheckedShiftL# w6# 40#) `or#`
                            (uncheckedShiftL# w5# 32#) `or#`
                                (uncheckedShiftL# w4# 24#) `or#`
                                    (uncheckedShiftL# w3# 16#) `or#`
                                        (uncheckedShiftL# w2# 8#) `or#` w1#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Word64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
-- on X86 we use bswap
-- TODO: find out if arch64 support this
#if (defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)) && defined(USE_BSWAP)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (W64# x#)) = writeWord8ArrayAsWord64# mba# i# (byteSwap64# x#)
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord64# mba# i# s0
        in (# s1, BE (W64# (byteSwap64# x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = BE (W64# (byteSwap64# (indexWord8ArrayAsWord64# ba# i#)))
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (W64# x#)) s0# =
        let s1# = writeWord8Array# mba# i# (uncheckedShiftRL# x# 56#) s0#
            s2# = writeWord8Array# mba# (i# +# 1#) (uncheckedShiftRL# x# 48#) s1#
            s3# = writeWord8Array# mba# (i# +# 2#) (uncheckedShiftRL# x# 40#) s2#
            s4# = writeWord8Array# mba# (i# +# 3#) (uncheckedShiftRL# x# 32#) s3#
            s5# = writeWord8Array# mba# (i# +# 4#) (uncheckedShiftRL# x# 24#) s4#
            s6# = writeWord8Array# mba# (i# +# 5#) (uncheckedShiftRL# x# 16#) s5#
            s7# = writeWord8Array# mba# (i# +# 6#) (uncheckedShiftRL# x# 8#) s6#
        in        writeWord8Array# mba# (i# +# 7#) x# s7#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, w8# #) = readWord8Array# mba# i# s0
            !(# s2, w7# #) = readWord8Array# mba# (i# +# 1#) s1
            !(# s3, w6# #) = readWord8Array# mba# (i# +# 2#) s2
            !(# s4, w5# #) = readWord8Array# mba# (i# +# 3#) s3
            !(# s5, w4# #) = readWord8Array# mba# (i# +# 4#) s4
            !(# s6, w3# #) = readWord8Array# mba# (i# +# 5#) s5
            !(# s7, w2# #) = readWord8Array# mba# (i# +# 6#) s6
            !(# s8, w1# #) = readWord8Array# mba# (i# +# 7#) s7
        in (# s8, BE (W64# ((uncheckedShiftL# w8# 56#) `or#`
                    (uncheckedShiftL# w7# 48#) `or#`
                        (uncheckedShiftL# w6# 40#) `or#`
                            (uncheckedShiftL# w5# 32#) `or#`
                                (uncheckedShiftL# w4# 24#) `or#`
                                    (uncheckedShiftL# w3# 16#) `or#`
                                        (uncheckedShiftL# w2# 8#) `or#` w1#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let w8# = indexWord8Array# ba# i#
            w7# = indexWord8Array# ba# (i# +# 1#)
            w6# = indexWord8Array# ba# (i# +# 2#)
            w5# = indexWord8Array# ba# (i# +# 3#)
            w4# = indexWord8Array# ba# (i# +# 4#)
            w3# = indexWord8Array# ba# (i# +# 5#)
            w2# = indexWord8Array# ba# (i# +# 6#)
            w1# = indexWord8Array# ba# (i# +# 7#)
        in BE (W64# ((uncheckedShiftL# w8# 56#) `or#`
                    (uncheckedShiftL# w7# 48#) `or#`
                        (uncheckedShiftL# w6# 40#) `or#`
                            (uncheckedShiftL# w5# 32#) `or#`
                                (uncheckedShiftL# w4# 24#) `or#`
                                    (uncheckedShiftL# w3# 16#) `or#`
                                        (uncheckedShiftL# w2# 8#) `or#` w1#))
#endif
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Word where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#endif
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (W# x#) = writeWord8ArrayAsWord# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWord# mba# i# s0 in (# s1, W# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = W# (indexWord8ArrayAsWord# ba# i#)

instance UnalignedAccess (LE Word) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (W# x#)) = writeWord8ArrayAs# mba# i# (LE (W32# x#))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (W32# x#) #) = readWord8ArrayAs# mba# i# s0 in (# s1, LE (W# x#) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = case (indexWord8ArrayAs# ba# i#) of (LE (W32# x#)) -> LE (W# x#)
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (W# x#)) = writeWord8ArrayAs# mba# i# (LE (W64# x#))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (W64# x#) #) = readWord8ArrayAs# mba# i# s0 in (# s1, LE (W# x#) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = case (indexWord8ArrayAs# ba# i#) of (LE (W64# x#)) -> LE (W# x#)
#endif

instance UnalignedAccess (BE Word) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (W# x#)) = writeWord8ArrayAs# mba# i# (BE (W32# x#))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (W32# x#) #) = readWord8ArrayAs# mba# i# s0 in (# s1, BE (W# x#) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = case (indexWord8ArrayAs# ba# i#) of (BE (W32# x#)) -> BE (W# x#)
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (W# x#)) = writeWord8ArrayAs# mba# i# (BE (W64# x#))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (W64# x#) #) = readWord8ArrayAs# mba# i# s0 in (# s1, BE (W# x#) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = case (indexWord8ArrayAs# ba# i#) of (BE (W64# x#)) -> BE (W# x#)
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Int16 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (I16# x#) = writeWord8ArrayAsInt16# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt16# mba# i# s0 in (# s1, I16# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = I16# (indexWord8ArrayAsInt16# ba# i#)

instance UnalignedAccess (LE Int16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (I16# x#)) =
        writeWord8ArrayAs# mba# i# (LE (W16# (int2Word# x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (W16# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, LE (I16# (narrow16Int# (word2Int# x#))) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let LE (W16# x#) = indexWord8ArrayAs# ba# i#
        in LE (I16# (narrow16Int# (word2Int# x#)))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Int16) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 2
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (I16# x#)) =
        writeWord8ArrayAs# mba# i# (BE (W16# (int2Word# x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (W16# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, BE (I16# (narrow16Int# (word2Int# x#))) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let !(BE (W16# x#)) = indexWord8ArrayAs# ba# i#
        in BE (I16# (narrow16Int# (word2Int# x#)))
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Int32 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (I32# x#) = writeWord8ArrayAsInt32# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt32# mba# i# s0 in (# s1, I32# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = I32# (indexWord8ArrayAsInt32# ba# i#)

instance UnalignedAccess (LE Int32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (I32# x#)) =
        writeWord8ArrayAs# mba# i# (LE (W32# (int2Word# x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (W32# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, LE (I32# (narrow32Int# (word2Int# x#))) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let LE (W32# x#) = indexWord8ArrayAs# ba# i#
        in LE (I32# (narrow32Int# (word2Int# x#)))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Int32) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (I32# x#)) =
        writeWord8ArrayAs# mba# i# (BE (W32# (int2Word# x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (W32# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, BE (I32# (narrow32Int# (word2Int# x#))) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let !(BE (W32# x#)) = indexWord8ArrayAs# ba# i#
        in BE (I32# (narrow32Int# (word2Int# x#)))
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Int64 where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (I64# x#) = writeWord8ArrayAsInt64# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt64# mba# i# s0 in (# s1, I64# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = I64# (indexWord8ArrayAsInt64# ba# i#)

instance UnalignedAccess (LE Int64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (I64# x#)) =
        writeWord8ArrayAs# mba# i# (LE (W64# (int2Word# x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (W64# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, LE (I64# (word2Int# x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let LE (W64# x#) = indexWord8ArrayAs# ba# i#
        in LE (I64# (word2Int# x#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Int64) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (I64# x#)) =
        writeWord8ArrayAs# mba# i# (BE (W64# (int2Word# x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (W64# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, BE (I64# (word2Int# x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let !(BE (W64# x#)) = indexWord8ArrayAs# ba# i#
        in BE (I64# (word2Int# x#))
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Int where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#endif
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (I# x#) = writeWord8ArrayAsInt# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsInt# mba# i# s0 in (# s1, I# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = I# (indexWord8ArrayAsInt# ba# i#)

instance UnalignedAccess (LE Int) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (I# x#)) = writeWord8ArrayAs# mba# i# (LE (I32# x#))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (I32# x#) #) = readWord8ArrayAs# mba# i# s0 in (# s1, LE (I# x#) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = case (indexWord8ArrayAs# ba# i#) of (LE (I32# x#)) -> LE (I# x#)
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (I# x#)) = writeWord8ArrayAs# mba# i# (LE (I64# x#))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (I64# x#) #) = readWord8ArrayAs# mba# i# s0 in (# s1, LE (I# x#) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = case (indexWord8ArrayAs# ba# i#) of (LE (I64# x#)) -> LE (I# x#)
#endif

instance UnalignedAccess (BE Int) where
#if SIZEOF_HSWORD == 4
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (I# x#)) = writeWord8ArrayAs# mba# i# (BE (I32# x#))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (I32# x#) #) = readWord8ArrayAs# mba# i# s0 in (# s1, BE (I# x#) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = case (indexWord8ArrayAs# ba# i#) of (BE (I32# x#)) -> BE (I# x#)
#else
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (I# x#)) = writeWord8ArrayAs# mba# i# (BE (I64# x#))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (I64# x#) #) = readWord8ArrayAs# mba# i# s0 in (# s1, BE (I# x#) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = case (indexWord8ArrayAs# ba# i#) of (BE (I64# x#)) -> BE (I# x#)
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Float where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (F# x#) = writeWord8ArrayAsFloat# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsFloat# mba# i# s0 in (# s1, F# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = F# (indexWord8ArrayAsFloat# ba# i#)

instance UnalignedAccess (LE Float) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (F# x#)) =
        writeWord8ArrayAs# mba# i# (LE (W32# (stgFloatToWord32 x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (W32# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, LE (F# (stgWord32ToFloat x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let LE (W32# x#) = indexWord8ArrayAs# ba# i#
        in LE (F# (stgWord32ToFloat x#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Float) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (F# x#)) =
        writeWord8ArrayAs# mba# i# (BE (W32# (stgFloatToWord32 x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (W32# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, BE (F# (stgWord32ToFloat x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let !(BE (W32# x#)) = indexWord8ArrayAs# ba# i#
        in BE (F# (stgWord32ToFloat x#))
#endif

--------------------------------------------------------------------------------

instance UnalignedAccess Double where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (D# x#) = writeWord8ArrayAsDouble# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsDouble# mba# i# s0 in (# s1, D# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = D# (indexWord8ArrayAsDouble# ba# i#)

instance UnalignedAccess (LE Double) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 8
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (D# x#)) =
        writeWord8ArrayAs# mba# i# (LE (W64# (stgDoubleToWord64 x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (W64# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, LE (D# (stgWord64ToDouble x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let LE (W64# x#) = indexWord8ArrayAs# ba# i#
        in LE (D# (stgWord64ToDouble x#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Double) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (D# x#)) =
        writeWord8ArrayAs# mba# i# (BE (W64# (stgDoubleToWord64 x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (W64# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, BE (D# (stgWord64ToDouble x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let !(BE (W64# x#)) = indexWord8ArrayAs# ba# i#
        in BE (D# (stgWord64ToDouble x#))
#endif

--------------------------------------------------------------------------------

-- | Char's instance use 31bit wide char prim-op.
instance UnalignedAccess Char where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (C# x#) = writeWord8ArrayAsWideChar# mba# i# x#
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, x# #) = readWord8ArrayAsWideChar# mba# i# s0 in (# s1, C# x# #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# = C# (indexWord8ArrayAsWideChar# ba# i#)

instance UnalignedAccess (LE Char) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (LE (C# x#)) =
        writeWord8ArrayAs# mba# i# (LE (I32# (ord# x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, LE (I32# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, LE (C# (chr# x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let LE (I32# x#) = indexWord8ArrayAs# ba# i#
        in LE (C# (chr# x#))
#else
    USE_HOST_IMPL(LE)
#endif

instance UnalignedAccess (BE Char) where
    {-# INLINE unalignedSize #-}
    unalignedSize = UnalignedSize 4
#if defined(WORDS_BIGENDIAN) || defined(USE_SHIFT)
    USE_HOST_IMPL(BE)
#else
    {-# INLINE writeWord8ArrayAs# #-}
    writeWord8ArrayAs# mba# i# (BE (C# x#)) =
        writeWord8ArrayAs# mba# i# (BE (I32# (ord# x#)))
    {-# INLINE readWord8ArrayAs# #-}
    readWord8ArrayAs# mba# i# s0 =
        let !(# s1, BE (I32# x#) #) = readWord8ArrayAs# mba# i# s0
        in (# s1, BE (C# (chr# x#)) #)
    {-# INLINE indexWord8ArrayAs# #-}
    indexWord8ArrayAs# ba# i# =
        let !(BE (I32# x#)) = indexWord8ArrayAs# ba# i#
        in BE (C# (chr# x#))
#endif

--------------------------------------------------------------------------------

-- Prim instances for newtypes in Foreign.C.Types
deriving instance UnalignedAccess CChar
deriving instance UnalignedAccess CSChar
deriving instance UnalignedAccess CUChar
deriving instance UnalignedAccess CShort
deriving instance UnalignedAccess CUShort
deriving instance UnalignedAccess CInt
deriving instance UnalignedAccess CUInt
deriving instance UnalignedAccess CLong
deriving instance UnalignedAccess CULong
deriving instance UnalignedAccess CPtrdiff
deriving instance UnalignedAccess CSize
deriving instance UnalignedAccess CWchar
deriving instance UnalignedAccess CSigAtomic
deriving instance UnalignedAccess CLLong
deriving instance UnalignedAccess CULLong
deriving instance UnalignedAccess CBool
deriving instance UnalignedAccess CIntPtr
deriving instance UnalignedAccess CUIntPtr
deriving instance UnalignedAccess CIntMax
deriving instance UnalignedAccess CUIntMax
deriving instance UnalignedAccess CClock
deriving instance UnalignedAccess CTime
deriving instance UnalignedAccess CUSeconds
deriving instance UnalignedAccess CSUSeconds
deriving instance UnalignedAccess CFloat
deriving instance UnalignedAccess CDouble
