{-|
Module      :  Z.Data.Prim.UnliftedRef
Description :  Primitive references
Copyright   :  (c) Dong Han 2017~2019
License     :  BSD-style

Maintainer  :  winterland1989@gmail.com
Stability   :  experimental
Portability :  portable

This package provide fast primitive references for primitive monad, such as ST or IO. Unboxed reference is implemented using single cell @MutableByteArray\/MutableUnliftedArray@ s to eliminate indirection overhead which MutVar# s a carry, on the otherhand primitive reference only support limited type(instances of 'Prim\/PrimUnlifted' class).
-}


module Z.Data.Ref.UnliftedRef
  ( -- * Unlifted references
    UnliftedRef(..)
  , PrimUnlifted(..)
  , newUnliftedRef
  , readUnliftedRef
  , writeUnliftedRef
  , modifyUnliftedRef
  ) where

import Control.Monad.Primitive
import Z.Data.Array.UnliftedArray

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
