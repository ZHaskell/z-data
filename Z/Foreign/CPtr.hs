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
    CPtr, newCPtr', newCPtrUnsafe, newCPtr
  , withCPtr, withCPtrsUnsafe, withCPtrForever, withCPtrs
  , addCPtrDep
  -- * Ptr type
  , Ptr
  , nullPtr
  , FunPtr
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import qualified Z.Data.Text                as T
import GHC.Ptr
import GHC.Exts
import GHC.IO
import Z.Data.Array                         hiding (newPinnedPrimArray)
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
{-# INLINABLE newCPtr' #-}
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
{-# INLINABLE newCPtrUnsafe #-}
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
{-# INLINABLE newCPtr #-}
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

-- | Use 'CPtr' as a 'Ptr' in FFI.
withCPtr :: CPtr a -> (Ptr a -> IO b) -> IO b
{-# INLINABLE withCPtr #-}
withCPtr (CPtr pa@(PrimArray ba#)) f = do
    r <- f (indexPrimArray pa 0)
    primitive_ (touch# ba#)
    return r

-- | Use 'CPtr' as a 'Ptr' in FFI(potentially running forever).
--
-- When you pass a forever loop to 'withCPtr', GHC's simplifier may think the 'touch#'(which keep the 'CPtr' alive) after the loop are unreachable,
-- so it may be optimized away, 'withCPtrForever' solves that.
--
withCPtrForever :: CPtr a -> (Ptr a -> IO b) -> IO b
#if MIN_VERSION_base(4,15,0)
{-# INLINABLE withCPtrForever #-}
withCPtrForever (CPtr pa@(PrimArray ba#)) f = IO $ \ s ->
    case f (indexPrimArray pa 0) of
        IO action# -> keepAlive# ba# s action#
#else
{-# NOINLINE withCPtrForever #-}
withCPtrForever (CPtr pa@(PrimArray ba#)) f = do
    r <- f (indexPrimArray pa 0)
    primitive_ (touch# ba#)
    return r
#endif

-- | Pass a list of 'CPtr Foo' as @foo**@. USE THIS FUNCTION WITH UNSAFE FFI ONLY!
withCPtrsUnsafe :: forall a b. [CPtr a] -> (BA# (Ptr a) -> Int -> IO b) -> IO b
{-# INLINABLE withCPtrsUnsafe #-}
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
{-# INLINABLE withCPtrs #-}
withCPtrs cptrs f = do
    mpa <- newPinnedPrimArray @IO @(Ptr a) len
    foldM_ (\ !i (CPtr pa) ->
        writePrimArray mpa i (indexPrimArray pa 0) >> return (i+1)) 0 cptrs
    r <- withMutablePrimArrayContents mpa $ \ p -> f p len
    primitive_ (touch# cptrs)
    return r
  where len = length cptrs

-- | @addCPtrDep a b@ make @b@\'s life depends on @a@\'s, so that @b@ is guaranteed to outlive @a@.
--
-- This function is useful when you want to create life dependency among 'CPtr's,
-- be careful about the cost of collecting weak pointers though, see
-- <http://blog.ezyang.com/2014/05/the-cost-of-weak-pointers-and-finalizers-in-ghc/>.
-- e.g. If three 'CPtr's form a dependency chain, the dead weak list may get traversed three times.
-- So instead of adding dependencies in a chain or one by one, you should add them in a single call
-- with a tuple like @addCPtrDep root (fieldA, fieldB, ...)@.
--
addCPtrDep :: CPtr a -> b -> IO ()
{-# INLINABLE addCPtrDep #-}
addCPtrDep (CPtr (PrimArray ba#)) b =
    primitive_ $ \ s0# ->
        let !(# s1#, _ #) = mkWeakNoFinalizer# ba# b s0#
        in s1#
