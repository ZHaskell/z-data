{-|
Module      : Z.Data.Foreign
Description : Use PrimArray \/ PrimVector with FFI
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide functions for using 'PrimArray' and 'PrimVector' with GHC FFI(Foreign function interface),
Some functions are designed to be used with <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ffi-chap.html#unlifted-ffi-types UnliftedFFITypes> extension.

GHC runtime is garbaged collected, there're two types of primitive array in GHC, with the objective to minimize overall memory management cost:

  * Small primitive arrays created with 'newArray' are directly allocated on GHC heap, which can be moved
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

module Z.Data.Foreign
  ( -- ** Type helper
    MBA#, BA#
    -- ** Unsafe FFI
  , withPrimArrayUnsafe
  , withPrimArrayListUnsafe
  , allocPrimArrayUnsafe
  , withPrimVectorUnsafe
  , allocPrimVectorUnsafe
  , withPrimUnsafe
  , allocPrimUnsafe
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
  , clearByteArray
  , clearPtr
  , castPtr
  , fromNullTerminated, fromPtr, fromPrimPtr
  , indexOffPtr, readOffPtr, writeOffPtr, nullPtr, advancePtr, subtractPtr
  , StdString, fromStdString
  -- ** re-export
  , module Foreign.C.Types
  , module Z.Data.Utils.Unaligned
  , module Z.Data.Array
  -- ** Internal helpers
  , z_std_string_size
  , z_copy_std_string
  , z_delete_std_string
  ) where

import           Control.Monad
import qualified Data.List                      as List
import           Data.Word
import           Data.Primitive.Ptr
import           Foreign.C.Types
import           GHC.Exts
import           GHC.ST
import           GHC.Ptr
import           Z.Data.Array                  
import           Z.Data.Utils.Unaligned
import           Z.Data.Vector.Base

-- | Type alias for 'ByteArray#', the type variable 'a' is for tagging pointer type only.
type BA# a = ByteArray#

-- | Type alias for 'MutableByteArray#', the type variable 'a' is for tagging pointer type only.
type MBA# a = MutableByteArray# RealWorld

-- | Pass primitive array to unsafe FFI as pointer.
--
-- Enable 'UnliftedFFITypes' extension in your haskell code, use proper pointer type and @HsInt@
-- to marshall @ByteArray#@ and @Int@ arguments on C side.
--
-- The second 'Int' arguement is the element size not the bytes size.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
withPrimArrayUnsafe :: (Prim a) => PrimArray a -> (BA# a -> Int -> r) -> r
{-# INLINE withPrimArrayUnsafe #-}
withPrimArrayUnsafe pa@(PrimArray ba#) f = f ba# (sizeofArray pa)

-- | Pass primitive array list to unsafe FFI as @StgArrBytes**@.
--
-- Enable 'UnliftedFFITypes' extension in your haskell code, use @StgArrBytes**@(>=8.10)
-- or @StgMutArrPtrs*@(<8.10) pointer type and @HsInt@
-- to marshall @Array# ByteArray#@ and @Int@ arguments on C side, check the example with 'Array# ByteArray#'.
--
-- The second 'Int' arguement is the list size.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
withPrimArrayListUnsafe :: [PrimArray a] -> (Array# (BA# a) -> Int -> b) -> b
{-# INLINE withPrimArrayListUnsafe #-}
withPrimArrayListUnsafe pas f =
    let l = List.length pas
    in case runST (do
        mla <- newArray l
        foldM_ (\ !i pa -> writeArray mla i pa >> return (i+1)) 0 pas
        unsafeFreezeArray mla
    ) of UnliftedArray la# -> f la# l

-- | Allocate some bytes and pass to FFI as pointer, freeze result into a 'PrimArray'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
allocPrimArrayUnsafe 
    :: forall a b. Prim a
    => Int 
    -> (MBA# a -> IO b)
    -> IO (PrimArray a, b)
{-# INLINE allocPrimArrayUnsafe #-}
allocPrimArrayUnsafe len f = do
    (mpa@(MutablePrimArray mba#) :: MutablePrimArray RealWorld a) <- newArray len
    !r <- f mba#
    !pa <- unsafeFreezeArray mpa
    return (pa, r)

-- | Pass 'PrimVector' to unsafe FFI as pointer
--
-- The 'PrimVector' version of 'withPrimArrayUnsafe'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimVectorUnsafe
    :: (Prim a)
    => PrimVector a
    -> (BA# a -> Int -> Int -> r) -- ^ array, start offset, end offset
    -> r
{-# INLINE withPrimVectorUnsafe #-}
withPrimVectorUnsafe (PrimVector arr s e) f =
    withPrimArrayUnsafe arr $ \ ba# _ -> f ba# s e

-- | Allocate a prim array and pass to FFI as pointer, freeze result into a 'PrimVector'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
allocPrimVectorUnsafe
    :: forall a b. Prim a
    => Int  -- ^ number of elements
    -> (MBA# a -> IO b)
    -> IO (PrimVector a, b)
{-# INLINE allocPrimVectorUnsafe #-}
allocPrimVectorUnsafe len f = do
    (mpa@(MutablePrimArray mba#) :: MutablePrimArray RealWorld a) <- newArray len
    !r <- f mba#
    !pa <- unsafeFreezeArray mpa
    let !v = PrimVector pa 0 len
    return (v, r)

-- | Create an one element primitive array and use it as a pointer to the primitive element.
--
-- Return the element and the computation result.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withPrimUnsafe
    :: (Prim a)
    => a            -- ^ initial value 
    -> (MBA# a -> IO b)
    -> IO (a, b)
{-# INLINE withPrimUnsafe #-}
withPrimUnsafe v f = do
    mpa@(MutablePrimArray mba#) <- newArray 1       -- All heap objects are WORD aligned
    writeArray mpa 0 v
    !b <- f mba#                                        -- so no need to do extra alignment
    !a <- readArray mpa 0
    return (a, b)

-- | like 'withPrimUnsafe', but don't write initial value.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
allocPrimUnsafe 
    :: (Prim a)
    => (MBA# a -> IO b)
    -> IO (a, b)
{-# INLINE allocPrimUnsafe #-}
allocPrimUnsafe f = do
    mpa@(MutablePrimArray mba#) <- newArray 1       -- All heap objects are WORD aligned
    !b <- f mba#                                    -- so no need to do extra alignment
    !a <- readArray mpa 0
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
{-# INLINABLE withPrimArraySafe #-}
withPrimArraySafe arr f
    | isPrimArrayPinned arr = do
        let siz = sizeofArray arr
        withPrimArrayContents arr $ \ ptr -> f ptr siz
    | otherwise = do
        let siz = sizeofArray arr
        buf <- newPinnedPrimArray siz
        copyArray buf 0 arr 0 siz
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
        pa <- unsafeFreezeArray ptrs
        withPrimArraySafe pa f
    go ptrs !i (pa:pas) =
        -- It's important to nest 'withPrimArraySafe' calls to keep all pointers alive
        withPrimArraySafe pa $ \ ppa _ -> do
            writeArray ptrs i ppa
            go ptrs (i+1) pas

-- | Allocate a prim array and pass to FFI as pointer, freeze result into a 'PrimVector'.
allocPrimArraySafe :: forall a b . Prim a
                    => Int      -- ^ in elements
                    -> (Ptr a -> IO b)
                    -> IO (PrimArray a, b)
{-# INLINABLE allocPrimArraySafe #-}
allocPrimArraySafe len f = do
    mpa <- newAlignedPinnedPrimArray len
    !r <- withMutablePrimArrayContents mpa f
    !pa <- unsafeFreezeArray mpa
    return (pa, r)

-- | Pass 'PrimVector' to safe FFI as pointer
--
-- The 'PrimVector' version of 'withPrimArraySafe'. The 'Ptr' is already pointed
-- to the first element, thus no offset is provided. After call returned, pointer is no longer valid.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimVectorSafe
    :: forall a b. Prim a
    => PrimVector a
    -> (Ptr a -> Int -> IO b)   -- ^ address, length in elements
    -> IO b
{-# INLINABLE withPrimVectorSafe #-}
withPrimVectorSafe (PrimVector arr s e) f
    | isPrimArrayPinned arr =
        withPrimArrayContents arr $ \ ptr ->
            let ptr' = ptr `plusPtr` (s * siz) in f ptr' (e-s)
    | otherwise = do
        let l = e-s
        buf <- newPinnedPrimArray l
        copyArray buf 0 arr s l
        withMutablePrimArrayContents buf $ \ ptr -> f ptr l
  where
    siz = sizeOf (undefined :: a)

-- | Create an one element primitive array and use it as a pointer to the primitive element.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimSafe :: forall a b. Prim a => a -> (Ptr a -> IO b) -> IO (a, b)
{-# INLINABLE withPrimSafe #-}
withPrimSafe v f = do
    buf <- newAlignedPinnedPrimArray 1
    writeArray buf 0 v
    !b <- withMutablePrimArrayContents buf $ \ ptr -> f ptr
    !a <- readArray buf 0
    return (a, b)

-- | like 'withPrimSafe', but don't write initial value.
allocPrimSafe :: forall a b. Prim a => (Ptr a -> IO b) -> IO (a, b)
{-# INLINABLE allocPrimSafe #-}
allocPrimSafe f = do
    buf <- newAlignedPinnedPrimArray 1
    !b <- withMutablePrimArrayContents buf $ \ ptr -> f ptr
    !a <- readArray buf 0
    return (a, b)

-- | Allocate a prim array and pass to FFI as pointer, freeze result into a 'PrimVector'.
allocPrimVectorSafe :: forall a b . Prim a
                    => Int      -- ^ size in elements
                    -> (Ptr a -> IO b)
                    -> IO (PrimVector a, b)
{-# INLINABLE allocPrimVectorSafe #-}
allocPrimVectorSafe len f = do
    mpa <- newAlignedPinnedPrimArray len
    !r <- withMutablePrimArrayContents mpa f
    !pa <- unsafeFreezeArray mpa
    let !v = PrimVector pa 0 len
    return (v, r)

-- | Allocate some bytes and pass to FFI as pointer, freeze result into a 'PrimVector'.
allocBytesSafe :: Int      -- ^ in bytes
               -> (Ptr Word8 -> IO b) 
               -> IO (Bytes, b)
{-# INLINABLE allocBytesSafe #-}
allocBytesSafe = allocPrimVectorSafe

-- | Convert a 'PrimArray' to a pinned one(memory won't moved by GC), copy if necessary.
pinPrimArray :: Prim a
             => PrimArray a 
             -> IO (PrimArray a)
{-# INLINABLE pinPrimArray #-}
pinPrimArray arr
    | isPrimArrayPinned arr = return arr
    | otherwise = do
        let l = sizeofArray arr
        buf <- newPinnedPrimArray l
        copyArray buf 0 arr 0 l
        arr' <- unsafeFreezeArray buf
        return arr'

-- | Convert a 'PrimVector' to a pinned one(memory won't moved by GC), copy if necessary.
pinPrimVector :: Prim a => PrimVector a -> IO (PrimVector a)
{-# INLINABLE pinPrimVector #-}
pinPrimVector v@(PrimVector pa s l)
    | isPrimArrayPinned pa = return v
    | otherwise = do
        buf <- newPinnedPrimArray l
        copyArray buf 0 pa s l
        pa' <- unsafeFreezeArray buf
        return (PrimVector pa' 0 l)

--------------------------------------------------------------------------------

-- | Zero a structure.
--
-- The length should be given in bytes.
--
clearByteArray :: MBA# a -> IO ()
{-# INLINABLE clearByteArray #-}
clearByteArray arr = do
    let marr = MutablePrimArray arr :: MutablePrimArray RealWorld Word8
    siz <- sizeofMutableArray marr
    setArray marr 0 siz 0

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Zero a structure.
--
-- There's no 'Storable' or 'Prim' constraint on 'a' type, the length
-- should be given in bytes.
--
clearPtr :: Ptr a -> Int -> IO ()
{-# INLINABLE clearPtr #-}
clearPtr dest nbytes = memset dest 0 (fromIntegral nbytes)

-- | Copy some bytes from a null terminated pointer(without copying the null terminator).
--
-- You should consider using 'Z.Data.CBytes.CBytes' type for storing NULL terminated bytes first,
-- This method is provided if you really need to read 'Bytes', there's no encoding guarantee,
-- result could be any bytes sequence.
fromNullTerminated :: Ptr a -> IO Bytes
{-# INLINABLE fromNullTerminated #-}
fromNullTerminated (Ptr addr#) = do
    len <- fromIntegral <$> c_strlen addr#
    marr <- newArray len
    copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
    arr <- unsafeFreezeArray marr
    return (PrimVector arr 0 len)

-- | Copy some bytes from a pointer.
--
-- There's no encoding guarantee, result could be any bytes sequence.
fromPtr :: Ptr a -> Int -- ^ in bytes
        -> IO Bytes
{-# INLINABLE fromPtr #-}
fromPtr (Ptr addr#) len = do
    marr <- newArray len
    copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
    arr <- unsafeFreezeArray marr
    return (PrimVector arr 0 len)

-- | Copy some bytes from a pointer.
--
-- There's no encoding guarantee, result could be any bytes sequence.
fromPrimPtr :: forall a. Prim a
            => Ptr a -> Int -- ^  in elements
            -> IO (PrimVector a)
{-# INLINABLE fromPrimPtr #-}
fromPrimPtr (Ptr addr#) len = do
    marr <- newArray len
    copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
    arr <- unsafeFreezeArray marr
    return (PrimVector arr 0 len)

-- | C++ @std::string@ Pointer tag.
data StdString

-- | Run FFI and marshall @std::string*@ result into Haskell heap bytes,
-- then delete the memory pointed by @std::string*@.
fromStdString :: IO (Ptr StdString) -> IO Bytes
{-# INLINABLE fromStdString #-}
fromStdString f = do
    q <- f 
    siz <- z_std_string_size q
    (bs,_) <- allocPrimVectorUnsafe siz (z_copy_std_string q siz)
    z_delete_std_string q
    return bs

foreign import ccall unsafe z_std_string_size :: Ptr StdString -> IO Int
foreign import ccall unsafe z_copy_std_string :: Ptr StdString -> Int -> MBA# Word8 -> IO ()
foreign import ccall unsafe z_delete_std_string :: Ptr StdString -> IO ()
