{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#ifdef HADDOCK_LANG_CN
{-|
Module      : Z.Data.Array.Cast
Description : 底层类型数组的cast操作
Copyright   : (c) Dong Han, 2017
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

'Cast'类型类定义了一些字节占用相同（因此可以互相cast）的类型，你可以使用 'cast' 来完成 C++ 里的 @reinterpret_cast@ 的功能，对于某些类型来说（例如从 'Int64' cast 到 'Double'）并不是没有操作的（因为 CPU 里用来储存在 'Double' 的寄存器可能无法用来储存 'Int64'）。

'Cast' 也可以用来规范可以相互转换的数组类型，例如从 @PrimArray Int@ 'cast' 到 @PrimArray Word@，'cast' 本身不改变数据的二进制表示，因此对于数组来说是没有额外操作的，详见 'Z.Data.Array.castArray'。

'Coercible' 约束的类型也可以进行 'cast'。
-}
#else
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
#endif

#include "MachDeps.h"

module Z.Data.Array.Cast
    ( Cast(..)
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Int
import           GHC.Word
#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif
import           GHC.Float


#ifdef HADDOCK_LANG_CN
-- | 一些可以互相转换的，内存尺寸相同的底层类型
#else
-- | `Cast` between primitive types of the same size.
#endif
--
class Cast source destination where
    cast :: source -> destination

instance {-# INCOHERENT #-} Coercible a b => Cast a b where
    cast = coerce

instance Cast Int8  Word8 where
    cast (I8# i) = W8# (narrow8Word# (int2Word# i))
instance Cast Int16 Word16 where
    cast (I16# i) = W16# (narrow16Word# (int2Word# i))
instance Cast Int32 Word32 where
    cast (I32# i) = W32# (narrow32Word# (int2Word# i))
instance Cast Int64 Word64 where
#if WORD_SIZE_IN_BITS < 64
    cast (I64# i) = W64# (int64ToWord64# i)
#else
    cast (I64# i) = W64# (int2Word# i)
#endif
instance Cast Int   Word where
    cast (I# i) = W# (int2Word# i)

instance Cast Word8  Int8 where
    cast (W8# i) = I8# (narrow8Int# (word2Int# i))
instance Cast Word16 Int16 where
    cast (W16# i) = I16# (narrow16Int# (word2Int# i))
instance Cast Word32 Int32 where
    cast (W32# i) = I32# (narrow32Int# (word2Int# i))
instance Cast Word64 Int64 where
#if WORD_SIZE_IN_BITS < 64
    cast (W64# i) = I64# (word64ToInt64# i)
#else
    cast (W64# i) = I64# (word2Int# i)
#endif
instance Cast Word   Int where
    cast (W# w) = I# (word2Int# w)

instance Cast Word64 Double where
    cast = castWord64ToDouble
instance Cast Word32 Float where
    cast = castWord32ToFloat
instance Cast Double Word64 where
    cast = castDoubleToWord64
instance Cast Float Word32 where
    cast = castFloatToWord32

instance Cast Int64 Double where
    cast = castWord64ToDouble . cast
instance Cast Int32 Float where
    cast = castWord32ToFloat . cast
instance Cast Double Int64 where
    cast = cast . castDoubleToWord64
instance Cast Float Int32 where
    cast = cast . castFloatToWord32
