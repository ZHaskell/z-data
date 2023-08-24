{-|
Module      :  Z.Data.Prim.PrimRef
Description :  Primitive references
Copyright   :  (c) Dong Han 2017~2019
License     :  BSD-style

Maintainer  :  winterland1989@gmail.com
Stability   :  experimental
Portability :  portable

This package provide fast primitive references for primitive monad, such as ST or IO. Unboxed reference is implemented using single cell @MutableByteArray\/MutableUnliftedArray@ s to eliminate indirection overhead which MutVar# s a carry, on the otherhand primitive reference only support limited type(instances of 'Prim\/PrimUnlifted' class).
-}


module Z.Data.Ref.PrimRef
  ( -- * Prim references
    PrimRef(..)
  , newPrimRef
  , readPrimRef
  , writePrimRef
  , modifyPrimRef
  , Prim(..)
    -- * Atomic operations for 'Counter'
  , Counter
    -- ** return value BEFORE atomic operation
  , atomicAddCounter
  , atomicSubCounter
  , atomicAndCounter
  , atomicNandCounter
  , atomicOrCounter
  , atomicXorCounter
    -- ** without returning
  , atomicAddCounter_
  , atomicSubCounter_
  , atomicAndCounter_
  , atomicNandCounter_
  , atomicOrCounter_
  , atomicXorCounter_
  ) where

import Control.Monad.Primitive
import Data.Primitive.Types
import Data.Primitive.PrimVar
import GHC.Exts
import GHC.IO

-- | A mutable variable in the 'PrimMonad' which can hold an instance of 'Prim'.
--
type PrimRef s a = PrimVar s a

-- | Build a new 'PrimRef'
--
newPrimRef :: (PrimMonad m, Prim a) => a -> m (PrimRef (PrimState m) a)
{-# INLINE newPrimRef #-}
newPrimRef = newPrimVar

-- | Read the value from an 'PrimRef'
--
readPrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> m a
readPrimRef (PrimRef mba) = readByteArray mba 0
{-# INLINE readPrimRef #-}

-- | Write a new value into an 'PrimRef'
--
writePrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> a -> m ()
writePrimRef (PrimRef mba) x = writeByteArray mba 0 x
{-# INLINE writePrimRef #-}

-- | Mutate the contents of an 'PrimRef'.
--
--  Unboxed reference is always strict on the value it hold.
--
modifyPrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> (a -> a) -> m ()
modifyPrimRef ref f = readPrimRef ref >>= writePrimRef ref . f
{-# INLINE modifyPrimRef #-}

--------------------------------------------------------------------------------

-- | Alias for 'PrimRef' 'RealWorld' 'Int' which support several atomic operations.
type Counter = PrimRef RealWorld Int

-- | Atomically add a 'Counter', return the value BEFORE added.
atomicAddCounter :: Counter -> Int -> IO Int
{-# INLINE atomicAddCounter #-}
atomicAddCounter (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchAddIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)

-- | Atomically add a 'Counter'.
atomicAddCounter_ :: Counter -> Int -> IO ()
atomicAddCounter_ (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, _ #) = fetchAddIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicAddCounter_ #-}

-- | Atomically sub a 'Counter', return the value BEFORE subbed.
atomicSubCounter :: Counter -> Int -> IO Int
{-# INLINE atomicSubCounter #-}
atomicSubCounter (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchSubIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)

-- | Atomically sub a 'Counter'
atomicSubCounter_ :: Counter -> Int -> IO ()
atomicSubCounter_ (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, _ #) = fetchSubIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicSubCounter_ #-}

-- | Atomically and a 'Counter', return the value BEFORE anded.
atomicAndCounter :: Counter -> Int -> IO Int
atomicAndCounter (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchAndIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)
{-# INLINE atomicAndCounter #-}

-- | Atomically and a 'Counter'
atomicAndCounter_ :: Counter -> Int -> IO ()
atomicAndCounter_ (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, _ #) = fetchAndIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicAndCounter_ #-}

-- | Atomically nand a 'Counter', return the value BEFORE nanded.
atomicNandCounter :: Counter -> Int -> IO Int
atomicNandCounter (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchNandIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)
{-# INLINE atomicNandCounter #-}

-- | Atomically nand a 'Counter'
atomicNandCounter_ :: Counter -> Int -> IO ()
atomicNandCounter_ (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, _ #) = fetchNandIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicNandCounter_ #-}

-- | Atomically or a 'Counter', return the value BEFORE ored.
atomicOrCounter :: Counter -> Int -> IO Int
atomicOrCounter (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchOrIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)
{-# INLINE atomicOrCounter #-}

-- | Atomically or a 'Counter'
atomicOrCounter_ :: Counter -> Int -> IO ()
atomicOrCounter_ (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, _ #) = fetchOrIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicOrCounter_ #-}

-- | Atomically xor a 'Counter', return the value BEFORE xored.
atomicXorCounter :: Counter -> Int -> IO Int
atomicXorCounter (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchXorIntArray# mba# 0# x# s1# in (# s2#, (I# res#) #)
{-# INLINE atomicXorCounter #-}

-- | Atomically xor a 'Counter'
atomicXorCounter_ :: Counter -> Int -> IO ()
atomicXorCounter_ (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, _ #) = fetchXorIntArray# mba# 0# x# s1# in (# s2#, () #)
{-# INLINE atomicXorCounter_ #-}
