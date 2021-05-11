{-|
Module      : Z.Foreign
Description : Use PrimArray \/ PrimVector with FFI
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide functions for using 'PrimArray' and 'PrimVector' with GHC FFI(Foreign function interface),
Some functions are designed to be used with <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ffi-chap.html#unlifted-ffi-types UnliftedFFITypes> extension.

GHC runtime is garbaged collected, there're two types of primitive array in GHC, with the objective to minimize overall memory management cost:

  * Small primitive arrays created with 'newPrimArray' are directly allocated on GHC heap, which can be moved
    by GHC garbage collector, we call these arrays @unpinned@. Allocating these array is cheap, we only need
    to check heap limit and bump heap pointer just like any other haskell heap objects. But we will pay GC cost
    , which is OK for small arrays.

  * Large primitive array and those created with 'newPinnedPrimArray' are allocated on GHC managed memory blocks,
    which is also traced by garbage collector, but will never moved before freed, thus are called @pinned@.
    Allocating these arrays are bit more expensive since it's more like how @malloc@ works, but we don't have to
    pay for GC cost.

Beside the @pinned/unpinned@ difference, we have two types of FFI calls in GHC:

  * Safe FFI call annotated with @safe@ keyword. These calls are executed on separated OS thread, which can be
    running concurrently with GHC garbage collector, thus we want to make sure only pinned arrays are passed.
    The main use case for @safe@ FFIs are long running functions, for example, doing IO polling.
    Since these calls are running on separated OS thread, haskell thread on original OS thread will not be affected.

  * Unsafe FFI call annotated with @unsafe@ keyword. These calls are executed on the same OS thread which is
    running the haskell side FFI code, which will in turn stop GHC from doing a garbage collection. We can pass
    both 'pinned' and 'unpinned' arrays in this case. The use case for @unsafe@ FFIs are short/small functions,
    which can be treated like a fat primitive operations, such as @memcpy@, @memcmp@. Using @unsafe@ FFI with
    long running functions will effectively block GHC runtime thread from running any other haskell threads, which
    is dangerous. Even if you use threaded runtime and expect your haskell thread can be stolen by other OS threads,
    but this will not work since GHC garbage collector will refuse to run if one of the OS thread is blocked by
    FFI calls.

Base on above analysis, we have following FFI strategy table.

  +--------------+---------------+---------------+
  | FFI  \ Array |    pinned     |   unpinned    |
  +--------------+---------------+---------------+
  |   unsafe     | directly pass | directly pass |
  +--------------+---------------+---------------+
  |     safe     | directly pass |  make a copy  |
  +--------------+---------------+---------------+

In this module, we separate safe and unsafe FFI handling due to the strategy difference: if the user can guarantee
a FFI call is unsafe, we can save an extra copy and pinned allocation. Mistakenly using unsafe function with safe FFI
will result in segfault.

-}

module Z.Foreign
  ( -- ** Unsafe FFI
    withPrimArrayUnsafe
  , allocPrimArrayUnsafe
  , withPrimVectorUnsafe
  , allocPrimVectorUnsafe
  , allocBytesUnsafe
  , withPrimUnsafe
  , allocPrimUnsafe
  , withPrimArrayListUnsafe
    -- ** Safe FFI
  , withPrimArraySafe
  , allocPrimArraySafe
  , withPrimVectorSafe
  , allocPrimVectorSafe
  , allocBytesSafe
  , withPrimSafe
  , allocPrimSafe
  , withPrimArrayListSafe
  , pinPrimArray
  , pinPrimVector
    -- ** Pointer helpers
  , BA#, MBA#, BAArray#
  , clearMBA
  , clearPtr
  , castPtr
  , fromNullTerminated, fromPtr, fromPrimPtr
  , StdString
  , peekStdString, peekStdStringIdx , peekStdStringN
  , fromStdString
  -- ** convert between bytestring
  , fromByteString
  , toByteString
  -- ** re-export
  , RealWorld
  , touch
  , module Data.Primitive.ByteArray
  , module Data.Primitive.PrimArray
  , module Foreign.C.Types
  , module Data.Primitive.Ptr
  , module Z.Data.Array.Unaligned
  -- ** Internal helpers
  , hs_std_string_size
  , hs_copy_std_string
  , hs_delete_std_string
  , hs_cal_std_string_off
  ) where

import           Control.Exception              (bracket)
import           Control.Monad
import           Control.Monad.Primitive
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import           Data.ByteString.Short.Internal (ShortByteString (..),
                                                 fromShort, toShort)
import qualified Data.ByteString.Unsafe         as B
import qualified Data.List                      as List
import           Data.Primitive
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Primitive.Ptr
import           Data.Word
import           Foreign.C.Types
import           GHC.Exts
import           GHC.Ptr
import           Z.Data.Array
import           Z.Data.Array.Unaligned
import           Z.Data.Array.UnliftedArray
import           Z.Data.Vector.Base

-- | Type alias for 'ByteArray#'.
--
-- Describe a 'ByteArray#' which we are going to pass across FFI. Use this type with @UnliftedFFITypes@
-- extension, At C side you should use a proper const pointer type.
--
-- Don't cast 'BA#' to 'Addr#' since the heap object offset is hard-coded in code generator:
-- <https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Foreign.hs#L542 Note [Unlifted boxed arguments to foreign calls]>
--
-- In haskell side we use type system to distinguish immutable / mutable arrays, but in C side we can't.
-- So it's users' responsibility to make sure the array content is not mutated (a const pointer type may help).
--
-- USE THIS TYPE WITH UNSAFE FFI CALL ONLY. A 'ByteArray#' COULD BE MOVED BY GC DURING SAFE FFI CALL.
type BA# a = ByteArray#

-- | Type alias for 'MutableByteArray#' 'RealWorld'.
--
-- Describe a 'MutableByteArray#' which we are going to pass across FFI. Use this type with @UnliftedFFITypes@
-- extension, At C side you should use a proper pointer type.
--
-- Don't cast 'MBA#' to 'Addr#' since the heap object offset is hard-coded in code generator:
-- <https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Foreign.hs#L542 Note [Unlifted boxed arguments to foreign calls]>
--
-- USE THIS TYPE WITH UNSAFE FFI CALL ONLY. A 'MutableByteArray#' COULD BE MOVED BY GC DURING SAFE FFI CALL.
type MBA# a = MutableByteArray# RealWorld

-- | Type alias for 'ArrayArray#'.
--
-- Describe a array of 'ByteArray#' which we are going to pass across FFI. Use this type with @UnliftedFFITypes@
-- extension, At C side you should use @StgArrBytes**@(>=8.10) or @StgMutArrPtrs*@(<8.10) type from "Rts.h",
-- example code modified from
-- <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ffi-chap.html#unlifted-ffi-types GHC manual>:
--
-- @
-- \/\/ C source, must include the RTS to make the struct StgArrBytes
-- \/\/ available along with its fields: ptrs and payload.
-- #include "Rts.h"
-- // GHC 8.10 changes the way how ArrayArray# is passed to C, so...
-- #if \_\_GLASGOW_HASKELL\_\_ < 810
-- HsInt sum_first (StgMutArrPtrs *arr, HsInt len) {
--   StgArrBytes **bufs = (StgArrBytes**)arr->payload;
-- #else
-- HsInt sum_first (StgArrBytes **bufs, HsInt len) {
-- #endif
--   int res = 0;
--   for(StgWord ix = 0;ix < len;ix++) {
--      // payload pointer type is StgWord*, cast it before use!
--      res = res + ((HsInt*)(bufs[ix]->payload))[0];
--   }
--   return res;
-- }
--
-- -- Haskell source, all elements in the argument array must be
-- -- either ByteArray\# or MutableByteArray\#. This is not enforced
-- -- by the type system in this example since ArrayArray is untyped.
-- foreign import ccall unsafe "sum_first" sumFirst :: BAArray# Int -> Int -> IO CInt
-- @
--
type BAArray# a = ArrayArray#

-- | Clear 'MBA#' with given length to zero.
clearMBA :: MBA# a
         -> Int  -- ^ in bytes
         -> IO ()
clearMBA mba# len = do
    let mba = (MutableByteArray mba#)
    setByteArray mba 0 len (0 :: Word8)

-- | Pass primitive array to unsafe FFI as pointer.
--
-- Enable 'UnliftedFFITypes' extension in your haskell code, use proper pointer type and @HsInt@
-- to marshall @ByteArray#@ and @Int@ arguments on C side.
--
-- The second 'Int' arguement is the element size not the bytes size.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
withPrimArrayUnsafe :: (Prim a) => PrimArray a -> (BA# a -> Int -> IO b) -> IO b
{-# INLINE withPrimArrayUnsafe #-}
withPrimArrayUnsafe pa@(PrimArray ba#) f = f ba# (sizeofPrimArray pa)

-- | Pass primitive array list to unsafe FFI as @StgArrBytes**@.
--
-- Enable 'UnliftedFFITypes' extension in your haskell code, use @StgArrBytes**@(>=8.10)
-- or @StgMutArrPtrs*@(<8.10) pointer type and @HsInt@
-- to marshall @BAArray#@ and @Int@ arguments on C side, check the example with 'BAArray#'.
--
-- The second 'Int' arguement is the list size.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
withPrimArrayListUnsafe :: [PrimArray a] -> (BAArray# a -> Int -> IO b) -> IO b
withPrimArrayListUnsafe pas f = do
    let l = List.length pas
    mla <- unsafeNewUnliftedArray l
    foldM_ (\ !i pa -> writeUnliftedArray mla i pa >> return (i+1)) 0 pas
    (UnliftedArray la#) <- unsafeFreezeUnliftedArray mla
    f la# l

-- | Allocate some bytes and pass to FFI as pointer, freeze result into a 'PrimArray'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
allocPrimArrayUnsafe :: forall a b. Prim a => Int -> (MBA# a -> IO b) -> IO (PrimArray a, b)
{-# INLINE allocPrimArrayUnsafe #-}
allocPrimArrayUnsafe len f = do
    (mpa@(MutablePrimArray mba#) :: MutablePrimArray RealWorld a) <- newPrimArray len
    !r <- f mba#
    !pa <- unsafeFreezePrimArray mpa
    return (pa, r)

-- | Pass 'PrimVector' to unsafe FFI as pointer
--
-- The 'PrimVector' version of 'withPrimArrayUnsafe'.
--
-- The second 'Int' arguement is the first element offset, the third 'Int' argument is the
-- element length.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimVectorUnsafe :: (Prim a)
                     => PrimVector a -> (BA# a -> Int -> Int -> IO b) -> IO b
{-# INLINE withPrimVectorUnsafe #-}
withPrimVectorUnsafe (PrimVector arr s l) f = withPrimArrayUnsafe arr $ \ ba# _ -> f ba# s l

-- | Allocate a prim array and pass to FFI as pointer, freeze result into a 'PrimVector'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
allocPrimVectorUnsafe :: forall a b. Prim a => Int  -- ^ number of elements
                      -> (MBA# a -> IO b) -> IO (PrimVector a, b)
{-# INLINE allocPrimVectorUnsafe #-}
allocPrimVectorUnsafe len f = do
    (mpa@(MutablePrimArray mba#) :: MutablePrimArray RealWorld a) <- newPrimArray len
    !r <- f mba#
    !pa <- unsafeFreezePrimArray mpa
    let !v = PrimVector pa 0 len
    return (v, r)

-- | Allocate some bytes and pass to FFI as pointer, freeze result into a 'Bytes'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
allocBytesUnsafe :: Int  -- ^ number of bytes
                 -> (MBA# a -> IO b) -> IO (Bytes, b)
{-# INLINE allocBytesUnsafe #-}
allocBytesUnsafe = allocPrimVectorUnsafe


-- | Create an one element primitive array and use it as a pointer to the primitive element.
--
-- Return the element and the computation result.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimUnsafe :: (Prim a)
               => a -> (MBA# a -> IO b) -> IO (a, b)
{-# INLINE withPrimUnsafe #-}
withPrimUnsafe v f = do
    mpa@(MutablePrimArray mba#) <- newPrimArray 1    -- All heap objects are WORD aligned
    writePrimArray mpa 0 v
    !b <- f mba#                                      -- so no need to do extra alignment
    !a <- readPrimArray mpa 0
    return (a, b)

-- | like 'withPrimUnsafe', but don't write initial value.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
allocPrimUnsafe :: (Prim a) => (MBA# a -> IO b) -> IO (a, b)
{-# INLINE allocPrimUnsafe #-}
allocPrimUnsafe f = do
    mpa@(MutablePrimArray mba#) <- newPrimArray 1    -- All heap objects are WORD aligned
    !b <- f mba#                                      -- so no need to do extra alignment
    !a <- readPrimArray mpa 0
    return (a, b)

--------------------------------------------------------------------------------

-- | Pass primitive array to safe FFI as pointer.
--
-- Use proper pointer type and @HsInt@ to marshall @Ptr a@ and @Int@ arguments on C side.
-- The memory pointed by 'Ptr a' will not moved during call. After call returned, pointer is no longer valid.
--
-- The second 'Int' arguement is the element size not the bytes size.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimArraySafe :: (Prim a) => PrimArray a -> (Ptr a -> Int -> IO b) -> IO b
{-# INLINE withPrimArraySafe #-}
withPrimArraySafe arr f
    | isPrimArrayPinned arr = do
        let siz = sizeofPrimArray arr
        withPrimArrayContents arr $ \ ptr -> f ptr siz
    | otherwise = do
        let siz = sizeofPrimArray arr
        buf <- newPinnedPrimArray siz
        copyPrimArray buf 0 arr 0 siz
        withMutablePrimArrayContents buf $ \ ptr -> f ptr siz

-- | Pass primitive array list to safe FFI as pointer.
--
-- Use proper pointer type and @HsInt@ to marshall @Ptr (Ptr a)@ and @Int@ arguments on C side.
-- The memory pointed by 'Ptr a' will not moved during call. After call returned, pointer is no longer valid.
--
-- The second 'Int' arguement is the list size.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimArrayListSafe :: Prim a => [PrimArray a] -> (Ptr (Ptr a) -> Int -> IO b) -> IO b
withPrimArrayListSafe pas0 f = do
    let l = List.length pas0
    ptrs <- newPinnedPrimArray l
    go ptrs 0 pas0
  where
    go ptrs !_ [] = do
        pa <- unsafeFreezePrimArray ptrs
        withPrimArraySafe pa f
    go ptrs !i (pa:pas) =
        -- It's important to nest 'withPrimArraySafe' calls to keep all pointers alive
        withPrimArraySafe pa $ \ ppa _ -> do
            writePrimArray ptrs i ppa
            go ptrs (i+1) pas

-- | Allocate a prim array and pass to FFI as pointer, freeze result into a 'PrimVector'.
allocPrimArraySafe :: forall a b . Prim a
                    => Int      -- ^ in elements
                    -> (Ptr a -> IO b)
                    -> IO (PrimArray a, b)
{-# INLINE allocPrimArraySafe #-}
allocPrimArraySafe len f = do
    mpa <- newAlignedPinnedPrimArray len
    !r <- withMutablePrimArrayContents mpa f
    !pa <- unsafeFreezePrimArray mpa
    return (pa, r)

-- | Pass 'PrimVector' to safe FFI as pointer
--
-- The 'PrimVector' version of 'withPrimArraySafe'. The 'Ptr' is already pointed
-- to the first element, thus no offset is provided. After call returned, pointer is no longer valid.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimVectorSafe :: forall a b. Prim a => PrimVector a -> (Ptr a -> Int -> IO b) -> IO b
{-# INLINE withPrimVectorSafe #-}
withPrimVectorSafe (PrimVector arr s l) f
    | isPrimArrayPinned arr =
        withPrimArrayContents arr $ \ ptr ->
            let ptr' = ptr `plusPtr` (s * siz) in f ptr' l
    | otherwise = do
        buf <- newPinnedPrimArray l
        copyPrimArray buf 0 arr s l
        withMutablePrimArrayContents buf $ \ ptr -> f ptr l
  where
    siz = sizeOf (undefined :: a)

-- | Create an one element primitive array and use it as a pointer to the primitive element.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimSafe :: forall a b. Prim a => a -> (Ptr a -> IO b) -> IO (a, b)
{-# INLINE withPrimSafe #-}
withPrimSafe v f = do
    buf <- newAlignedPinnedPrimArray 1
    writePrimArray buf 0 v
    !b <- withMutablePrimArrayContents buf $ \ ptr -> f ptr
    !a <- readPrimArray buf 0
    return (a, b)

-- | like 'withPrimSafe', but don't write initial value.
allocPrimSafe :: forall a b. Prim a => (Ptr a -> IO b) -> IO (a, b)
{-# INLINE allocPrimSafe #-}
allocPrimSafe f = do
    buf <- newAlignedPinnedPrimArray 1
    !b <- withMutablePrimArrayContents buf $ \ ptr -> f ptr
    !a <- readPrimArray buf 0
    return (a, b)

-- | Allocate a prim array and pass to FFI as pointer, freeze result into a 'PrimVector'.
allocPrimVectorSafe :: forall a b . Prim a
                    => Int      -- ^ in elements
                    -> (Ptr a -> IO b) -> IO (PrimVector a, b)
{-# INLINE allocPrimVectorSafe #-}
allocPrimVectorSafe len f = do
    mpa <- newAlignedPinnedPrimArray len
    !r <- withMutablePrimArrayContents mpa f
    !pa <- unsafeFreezePrimArray mpa
    let !v = PrimVector pa 0 len
    return (v, r)

-- | Allocate some bytes and pass to FFI as pointer, freeze result into a 'PrimVector'.
allocBytesSafe :: Int      -- ^ in bytes
               -> (Ptr Word8 -> IO b) -> IO (Bytes, b)
{-# INLINE allocBytesSafe #-}
allocBytesSafe = allocPrimVectorSafe

-- | Convert a 'PrimArray' to a pinned one(memory won't moved by GC) if necessary.
pinPrimArray :: Prim a => PrimArray a -> IO (PrimArray a)
{-# INLINE pinPrimArray #-}
pinPrimArray arr
    | isPrimArrayPinned arr = return arr
    | otherwise = do
        let l = sizeofPrimArray arr
        buf <- newPinnedPrimArray l
        copyPrimArray buf 0 arr 0 l
        arr' <- unsafeFreezePrimArray buf
        return arr'

-- | Convert a 'PrimVector' to a pinned one(memory won't moved by GC) if necessary.
pinPrimVector :: Prim a => PrimVector a -> IO (PrimVector a)
{-# INLINE pinPrimVector #-}
pinPrimVector v@(PrimVector pa s l)
    | isPrimArrayPinned pa = return v
    | otherwise = do
        buf <- newPinnedPrimArray l
        copyPrimArray buf 0 pa s l
        pa' <- unsafeFreezePrimArray buf
        return (PrimVector pa' 0 l)

--------------------------------------------------------------------------------

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Zero a structure.
--
-- There's no 'Storable' or 'Prim' constraint on 'a' type, the length
-- should be given in bytes.
--
clearPtr :: Ptr a -> Int -> IO ()
{-# INLINE clearPtr #-}
clearPtr dest nbytes = memset dest 0 (fromIntegral nbytes)

-- | Copy some bytes from a null terminated pointer(without copying the null terminator).
--
-- You should consider using 'Z.Data.CBytes.CBytes' type for storing NULL terminated bytes first,
-- This method is provided if you really need to read 'Bytes', there's no encoding guarantee,
-- result could be any bytes sequence.
fromNullTerminated :: Ptr a -> IO Bytes
{-# INLINE fromNullTerminated #-}
fromNullTerminated (Ptr addr#) = do
    len <- fromIntegral <$> c_strlen addr#
    marr <- newPrimArray len
    copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
    arr <- unsafeFreezePrimArray marr
    return (PrimVector arr 0 len)

-- | Copy some bytes from a pointer.
--
-- There's no encoding guarantee, result could be any bytes sequence.
fromPtr :: Ptr a -> Int -- ^ in bytes
        -> IO Bytes
{-# INLINE fromPtr #-}
fromPtr (Ptr addr#) len = do
    marr <- newPrimArray len
    copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
    arr <- unsafeFreezePrimArray marr
    return (PrimVector arr 0 len)

-- | Copy some bytes from a pointer.
--
-- There's no encoding guarantee, result could be any bytes sequence.
fromPrimPtr :: forall a. Prim a
            => Ptr a -> Int -- ^  in elements
            -> IO (PrimVector a)
{-# INLINE fromPrimPtr #-}
fromPrimPtr (Ptr addr#) len = do
    marr <- newPrimArray len
    copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
    arr <- unsafeFreezePrimArray marr
    return (PrimVector arr 0 len)

-- | @std::string@ Pointer tag.
data StdString

-- | Peek a pointer of std::string to 'Bytes'.
--
-- Note that we do NOT delete the pointee std::string. If you want to auto free
-- it, see 'fromStdString' instead.
peekStdString :: Ptr StdString -> IO Bytes
peekStdString ptr = do
    siz <- hs_std_string_size ptr
    (bs, _) <- allocBytesUnsafe siz (hs_copy_std_string ptr siz)
    return bs

peekStdStringIdx :: Ptr StdString -> Int -> IO Bytes
peekStdStringIdx p idx = peekStdString =<< hs_cal_std_string_off p idx

peekStdStringN :: Int -> Ptr StdString -> IO [Bytes]
peekStdStringN len ptr = forM [0..len-1] (peekStdStringIdx ptr)

-- | Run FFI in bracket and marshall @std::string*@ result into Haskell heap
-- bytes, memory pointed by @std::string*@ will be @delete@ ed.
fromStdString :: IO (Ptr StdString) -> IO Bytes
fromStdString f = bracket f hs_delete_std_string
    (\ q -> do
        siz <- hs_std_string_size q
        (bs,_) <- allocBytesUnsafe siz (hs_copy_std_string q siz)
        return bs)

foreign import ccall unsafe hs_std_string_size :: Ptr StdString -> IO Int
foreign import ccall unsafe hs_copy_std_string :: Ptr StdString -> Int -> MBA# Word8 -> IO ()
foreign import ccall unsafe hs_delete_std_string :: Ptr StdString -> IO ()
foreign import ccall unsafe hs_cal_std_string_off :: Ptr StdString -> Int -> IO (Ptr StdString)

-- | O(n), Convert from 'ByteString'.
fromByteString :: ByteString -> Bytes
fromByteString bs = case toShort bs of
    (SBS ba#) -> PrimVector (PrimArray ba#) 0 (B.length bs)

-- | O(n), Convert tp 'ByteString'.
toByteString :: Bytes -> ByteString
toByteString (PrimVector (PrimArray ba#) s l) = B.unsafeTake l . B.unsafeDrop s . fromShort $ SBS ba#
