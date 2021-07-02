{-|
Module      :  Z.Data.PrimRef.PrimRef
Description :  Primitive references
Copyright   :  (c) Dong Han 2017~2019
License     :  BSD-style

Maintainer  :  winterland1989@gmail.com
Stability   :  experimental
Portability :  portable

This package provide fast primitive references for primitive monad, such as ST or IO. Unboxed reference is implemented using single cell @MutableByteArray\/MutableUnliftedArray@ s to eliminate indirection overhead which MutVar# s a carry, on the otherhand primitive reference only support limited type(instances of 'Prim\/PrimUnlifted' class).
-}


module Z.Data.PrimRef
  ( -- * Prim references
    PrimRef(..), PrimIORef
  , newPrimRef
  , readPrimRef
  , writePrimRef
  , modifyPrimRef
  , Prim(..)
    -- * Unlifted references
  , UnliftedRef(..)
  , newUnliftedRef
  , readUnliftedRef
  , writeUnliftedRef
  , modifyUnliftedRef
  , PrimUnlifted(..)
    -- * Atomic operations for @PrimIORef Int@
  , Counter
  , newCounter
  , readCounter
  , writeCounter
  , modifyCounter
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

import Control.Monad.Primitive
import Data.Primitive.Types
import Data.Primitive.ByteArray
import GHC.Exts
import GHC.IO
import Z.Data.Array.UnliftedArray

-- | A mutable variable in the 'PrimMonad' which can hold an instance of 'Prim'.
--
newtype PrimRef s a = PrimRef (MutableByteArray s)

-- | Type alias for 'PrimRef' in IO.
type PrimIORef a = PrimRef RealWorld a

-- | Build a new 'PrimRef'
--
newPrimRef :: (Prim a, PrimMonad m) => a -> m (PrimRef (PrimState m) a)
newPrimRef x = do
     mba <- newByteArray (I# (sizeOf# x))
     writeByteArray mba 0 x
     return (PrimRef mba)
{-# INLINE newPrimRef #-}

-- | Read the value of an 'PrimRef'
--
readPrimRef :: (Prim a, PrimMonad m) => PrimRef (PrimState m) a -> m a
readPrimRef (PrimRef mba) = readByteArray mba 0
{-# INLINE readPrimRef #-}

-- | Write a new value into an 'PrimRef'
--
writePrimRef :: (Prim a, PrimMonad m) => PrimRef (PrimState m) a -> a -> m ()
writePrimRef (PrimRef mba) x = writeByteArray mba 0 x
{-# INLINE writePrimRef #-}

-- | Mutate the contents of an 'PrimRef'.
--
--  Unboxed reference is always strict on the value it hold.
--
modifyPrimRef :: (Prim a, PrimMonad m) => PrimRef (PrimState m) a -> (a -> a) -> m ()
modifyPrimRef ref f = readPrimRef ref >>= writePrimRef ref . f
{-# INLINE modifyPrimRef #-}

-- | Alias for 'PrimIORef Int' which support several atomic operations.
type Counter = PrimRef RealWorld Int

-- | Build a new 'Counter'
newCounter :: Int -> IO Counter
newCounter = newPrimRef
{-# INLINE newCounter #-}

-- | Read the value of an 'Counter'.
readCounter :: Counter -> IO Int
readCounter = readPrimRef
{-# INLINE readCounter #-}

-- | Write a new value into an 'Counter'(non-atomically).
writeCounter :: Counter -> Int -> IO ()
writeCounter = writePrimRef
{-# INLINE writeCounter #-}

-- | Mutate the contents of an 'Counter'(non-atomically).
modifyCounter :: Counter -> (Int -> Int) -> IO ()
modifyCounter = modifyPrimRef
{-# INLINE modifyCounter #-}

-- | Atomically add a 'Counter', return the value AFTER added.
atomicAddCounter' :: Counter -> Int -> IO Int
{-# INLINE atomicAddCounter' #-}
atomicAddCounter' (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchAddIntArray# mba# 0# x# s1# in (# s2#, (I# (res# +# x#)) #)

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


-- | Atomically sub a 'Counter', return the value AFTER subbed.
atomicSubCounter' :: Counter -> Int -> IO Int
{-# INLINE atomicSubCounter' #-}
atomicSubCounter' (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchSubIntArray# mba# 0# x# s1# in (# s2#, (I# (res# -# x#)) #)

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

-- | Atomically and a 'Counter', return the value AFTER anded.
atomicAndCounter' :: Counter -> Int -> IO Int
atomicAndCounter' (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchAndIntArray# mba# 0# x# s1# in (# s2#, (I# (res# `andI#` x#)) #)
{-# INLINE atomicAndCounter' #-}

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

-- | Atomically nand a 'Counter', return the value AFTER nanded.
atomicNandCounter' :: Counter -> Int -> IO Int
atomicNandCounter' (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchNandIntArray# mba# 0# x# s1# in (# s2#, (I# (notI# (res# `andI#` x#))) #)
{-# INLINE atomicNandCounter' #-}

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

-- | Atomically or a 'Counter', return the value AFTER ored.
atomicOrCounter' :: Counter -> Int -> IO Int
atomicOrCounter' (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchOrIntArray# mba# 0# x# s1# in (# s2#, (I# (res# `orI#` x#)) #)
{-# INLINE atomicOrCounter' #-}

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

-- | Atomically xor a 'Counter', return the value AFTER xored.
atomicXorCounter' :: Counter -> Int -> IO Int
atomicXorCounter' (PrimRef (MutableByteArray mba#)) (I# x#) = IO $ \ s1# ->
    let !(# s2#, res# #) = fetchXorIntArray# mba# 0# x# s1# in (# s2#, (I# (res# `xorI#` x#)) #)
{-# INLINE atomicXorCounter' #-}

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

-- | A mutable variable in the 'PrimMonad' which can hold an instance of 'PrimUnlifted'.
--
newtype UnliftedRef s a = UnliftedRef (MutableUnliftedArray s a)

-- | Build a new 'UnliftedRef'
--
newUnliftedRef :: (PrimUnlifted a, PrimMonad m) => a -> m (UnliftedRef (PrimState m) a)
newUnliftedRef x = do
     mba <- newUnliftedArray 1 x
     return (UnliftedRef mba)
{-# INLINE newUnliftedRef #-}

-- | Read the value of an 'UnliftedRef'
--
readUnliftedRef :: (PrimUnlifted a, PrimMonad m) => UnliftedRef (PrimState m) a -> m a
readUnliftedRef (UnliftedRef mba) = readUnliftedArray mba 0
{-# INLINE readUnliftedRef #-}

-- | Write a new value into an 'UnliftedRef'
--
writeUnliftedRef :: (PrimUnlifted a, PrimMonad m) => UnliftedRef (PrimState m) a -> a -> m ()
writeUnliftedRef (UnliftedRef mba) x = writeUnliftedArray mba 0 x
{-# INLINE writeUnliftedRef #-}

-- | Mutate the contents of an 'UnliftedRef'.
--
--  Unlifted reference is always strict on the value it hold.
--
modifyUnliftedRef :: (PrimUnlifted a, PrimMonad m) => UnliftedRef (PrimState m) a -> (a -> a) -> m ()
modifyUnliftedRef ref f = readUnliftedRef ref >>= writeUnliftedRef ref . f
{-# INLINE modifyUnliftedRef #-}
