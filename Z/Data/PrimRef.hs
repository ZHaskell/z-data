{-|
Module      :  Z.Data.PrimRef
Copyright   :  (c) Dong Han 2017~2019
License     :  BSD-style
Maintainer  :  winterland1989@gmail.com
Stability   :  experimental
Portability :  portable

This module provide fast unboxed references for ST and IO monad, and atomic operations for 'Counter' type. Unboxed reference is implemented using single cell MutableByteArray s to eliminate indirection overhead which MutVar# s a carry, on the otherhand unboxed reference only support limited type(instances of Prim class).

-}

module Z.Data.PrimRef
  ( -- * Unboxed ST references
    PrimSTRef
  , newPrimSTRef
  , readPrimSTRef
  , writePrimSTRef
  , modifyPrimSTRef
  , -- * Unboxed IO references
    PrimIORef
  , newPrimIORef
  , readPrimIORef
  , writePrimIORef
  , modifyPrimIORef
    -- * Atomic operations for @PrimIORef Int@
  , Counter
  , newCounter
    -- ** return value BEFORE atomic operation
  , atomicAddCounter
  , atomicSubCounter
  , atomicAndCounter
  , atomicNandCounter
  , atomicOrCounter
  , atomicXorCounter
    -- ** return value AFTER atomic operation
  , atomicAddCounter'
  , atomicSubCounter'
  , atomicAndCounter'
  , atomicNandCounter'
  , atomicOrCounter'
  , atomicXorCounter'
    -- ** without returning
  , atomicAddCounter_
  , atomicSubCounter_
  , atomicAndCounter_
  , atomicNandCounter_
  , atomicOrCounter_
  , atomicXorCounter_
  ) where

import Z.Data.PrimRef.PrimSTRef
import Z.Data.PrimRef.PrimIORef
