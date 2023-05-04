{-|
Module      : Z.Data.Array.Cast
Description : Primitive casting
Copyright   : Haskell Foundation, (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module is borrowed from basement's Cast module with conditional instances removed. The purpose of 'Cast' is to provide primitive types which share the same byte size, so that arrays and vectors parameterized by them can be safely coerced without breaking the index bounds. You can also use it to directly cast primitives just like @reinterpret_cast@. A 'Coercible' based instance is also provide for convenience.

-}

#include "MachDeps.h"

module Z.Data.Array.Cast
    ( Cast(..)
    ) where

import           GHC.Exts
import           GHC.Int
import           GHC.Word
#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif
import           GHC.Float

-- | `Cast` between primitive types of the same size.
--
class Cast source destination where
    cast :: source -> destination

instance {-# INCOHERENT #-} Coercible a b => Cast a b where
    cast = coerce

instance Cast Int8  Word8 where
    {-# INLINE cast #-}
    cast (I8# i) = W8# (int8ToWord8# i)
instance Cast Int16 Word16 where
    {-# INLINE cast #-}
    cast (I16# i) = W16# (int16ToWord16# i)
instance Cast Int32 Word32 where
    {-# INLINE cast #-}
    cast (I32# i) = W32# (int32ToWord32# i)
instance Cast Int64 Word64 where
    {-# INLINE cast #-}
    cast = int64ToWord64
instance Cast Int   Word where
    {-# INLINE cast #-}
    cast (I# i) = W# (int2Word# i)

instance Cast Word8  Int8 where
    {-# INLINE cast #-}
    cast (W8# i) = I8# (word8ToInt8# i)
instance Cast Word16 Int16 where
    {-# INLINE cast #-}
    cast (W16# i) = I16# (word16ToInt16# i)
instance Cast Word32 Int32 where
    {-# INLINE cast #-}
    cast (W32# i) = I32# (word32ToInt32# i)
instance Cast Word64 Int64 where
    {-# INLINE cast #-}
    cast = word64ToInt64
instance Cast Word   Int where
    {-# INLINE cast #-}
    cast (W# w) = I# (word2Int# w)

instance Cast Word64 Double where
    {-# INLINE cast #-}
    cast = castWord64ToDouble
instance Cast Word32 Float where
    {-# INLINE cast #-}
    cast = castWord32ToFloat
instance Cast Double Word64 where
    {-# INLINE cast #-}
    cast = castDoubleToWord64
instance Cast Float Word32 where
    {-# INLINE cast #-}
    cast = castFloatToWord32

instance Cast Int64 Double where
    {-# INLINE cast #-}
    cast = castWord64ToDouble . cast
instance Cast Int32 Float where
    {-# INLINE cast #-}
    cast = castWord32ToFloat . cast
instance Cast Double Int64 where
    {-# INLINE cast #-}
    cast = cast . castDoubleToWord64
instance Cast Float Int32 where
    {-# INLINE cast #-}
    cast = cast . castFloatToWord32

int64ToWord64 :: Int64 -> Word64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
int64ToWord64 (I64# i) = W64# (int64ToWord64# i)
#else
int64ToWord64 (I64# i) = W64# (int2Word# i)
#endif
#else
int64ToWord64 (I64# i) = W64# (int64ToWord64# i)
#endif

word64ToInt64 :: Word64 -> Int64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
word64ToInt64 (W64# i) = I64# (word64ToInt64# i)
#else
word64ToInt64 (W64# i) = I64# (word2Int# i)
#endif
#else
word64ToInt64 (W64# i) = I64# (word64ToInt64# i)
#endif
