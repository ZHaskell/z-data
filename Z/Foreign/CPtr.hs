{-|
Module      : Z.Foreign.CPtr
Description : Lightweight foreign pointer
Copyright   : (c) Dong Han, 2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide a lightweight foreign pointer, support c initializer and finalizer only.
-}

module Z.Foreign.CPtr (
  -- * CPtr type
    CPtr, newCPtrUnsafe, newCPtr, withCPtr
  -- * Ptr type
  , Ptr
  , nullPtr
  , FunPtr
  ) where

import Control.Monad.Primitive
import Control.Exception        (mask_)
import Data.Primitive.PrimArray
import Z.Data.Text              as T
import GHC.Ptr
import GHC.Exts

-- | Lightweight foreign pointers.
newtype CPtr a = CPtr (PrimArray (Ptr a))

instance Eq (CPtr a) where
    {-# INLINE (==) #-}
    CPtr a == CPtr b = indexPrimArray a 0 == indexPrimArray b 0

instance Ord (CPtr a) where
    {-# INLINE compare #-}
    CPtr a `compare` CPtr b = indexPrimArray a 0 `compare` indexPrimArray b 0

instance Show (CPtr a) where
    show = toString

instance T.Print (CPtr a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (CPtr mpa) = T.toUTF8BuilderP 0 (indexPrimArray mpa 0)

-- | Initialize a 'CPtr' with initializer(must be unsafe FFI) and finalizer.
--
-- The initializer will receive a pointer of pointer so that it can do allocation and
-- write pointer back.
newCPtrUnsafe :: (MutableByteArray# RealWorld -> IO r) -- ^ initializer
              -> FunPtr (Ptr a -> IO b) -- ^ finalizer
              -> IO (CPtr a, r)
newCPtrUnsafe ini (FunPtr fin#) = mask_ $ do
    mpa@(MutablePrimArray mba#) <- newPrimArray 1
    r <- ini mba#
    (Ptr addr#) <- readPrimArray mpa 0
    pa@(PrimArray ba#) <- unsafeFreezePrimArray mpa
    primitive_ $ \ s0# ->
        let !(# s1#, w# #) = mkWeakNoFinalizer# ba# () s0#
            !(# s2#, _ #) = addCFinalizerToWeak# fin# addr# 0# addr# w# s1#
        in s2#
    return (CPtr pa, r)

-- | Initialize a 'CPtr' with initializer and finalizer.
--
-- The initializer will receive a pointer of pointer so that it can do allocation and
-- write pointer back.
newCPtr :: (Ptr (Ptr a) -> IO r) -- ^ initializer
        -> FunPtr (Ptr a -> IO b) -- ^ finalizer
        -> IO (CPtr a, r)
newCPtr ini (FunPtr fin#) = mask_ $ do
    mpa <- newPinnedPrimArray 1
    r <- ini (mutablePrimArrayContents mpa)
    (Ptr addr#) <- readPrimArray mpa 0
    pa@(PrimArray ba#) <- unsafeFreezePrimArray mpa
    primitive_ $ \ s0# ->
        let !(# s1#, w# #) = mkWeakNoFinalizer# ba# () s0#
            !(# s2#, _ #) = addCFinalizerToWeak# fin# addr# 0# addr# w# s1#
        in s2#
    return (CPtr pa, r)

-- | The only way to use 'CPtr' as a 'Ptr' in FFI is to use 'withCPtr'.
withCPtr :: CPtr a -> (Ptr a -> IO b) -> IO b
withCPtr (CPtr pa@(PrimArray ba#)) f = do
    r <- f (indexPrimArray pa 0)
    primitive_ (touch# ba#)
    return r
