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
    CPtr, newCPtr', newCPtrUnsafe, newCPtr, withCPtr, withCPtrsUnsafe, withCPtrs
  -- * Ptr type
  , Ptr
  , nullPtr
  , FunPtr
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Exception                    (mask_)
import Data.Primitive.PrimArray
import qualified Z.Data.Text                as T
import GHC.Ptr
import GHC.Exts
import Z.Data.Array
import Z.Foreign

-- | Lightweight foreign pointers.
newtype CPtr a = CPtr (PrimArray (Ptr a))

instance Eq (CPtr a) where
    {-# INLINE (==) #-}
    CPtr a == CPtr b = indexPrimArray a 0 == indexPrimArray b 0

instance Ord (CPtr a) where
    {-# INLINE compare #-}
    CPtr a `compare` CPtr b = indexPrimArray a 0 `compare` indexPrimArray b 0

instance Show (CPtr a) where
    show = T.toString

instance T.Print (CPtr a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (CPtr mpa) = T.toUTF8BuilderP 0 (indexPrimArray mpa 0)

-- | Initialize a 'CPtr' with initializer which return an allocated pointer.
--
newCPtr' :: IO (Ptr a) -- ^ initializer
         -> FunPtr (Ptr a -> IO b) -- ^ finalizer
         -> IO (CPtr a)
newCPtr' ini (FunPtr fin#) = mask_ $ do
    mpa <- newPrimArray 1
    p@(Ptr addr#) <- ini
    writePrimArray mpa 0 p
    pa@(PrimArray ba#) <- unsafeFreezePrimArray mpa
    primitive_ $ \ s0# ->
        let !(# s1#, w# #) = mkWeakNoFinalizer# ba# () s0#
            !(# s2#, _ #) = addCFinalizerToWeak# fin# addr# 0# addr# w# s1#
        in s2#
    return (CPtr pa)

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

-- | Pass a list of 'CPtr Foo' as @foo**@. USE THIS FUNCTION WITH UNSAFE FFI ONLY!
withCPtrsUnsafe :: forall a b. [CPtr a] -> (BA# (Ptr a) -> Int -> IO b) -> IO b
withCPtrsUnsafe cptrs f = do
    mpa <- newPrimArray @IO @(Ptr a) len
    foldM_ (\ !i (CPtr pa) ->
        writePrimArray mpa i (indexPrimArray pa 0) >> return (i+1)) 0 cptrs
    (PrimArray ba#) <- unsafeFreezePrimArray mpa
    r <- f ba# len
    primitive_ (touch# cptrs)
    return r
  where len = length cptrs

-- | Pass a list of 'CPtr Foo' as @foo**@.
withCPtrs :: forall a b. [CPtr a] -> (Ptr (Ptr a) -> Int -> IO b) -> IO b
withCPtrs cptrs f = do
    mpa <- newPinnedPrimArray @IO @(Ptr a) len
    foldM_ (\ !i (CPtr pa) ->
        writePrimArray mpa i (indexPrimArray pa 0) >> return (i+1)) 0 cptrs
    r <- withMutablePrimArrayContents mpa $ \ p -> f p len
    primitive_ (touch# cptrs)
    return r
  where len = length cptrs
