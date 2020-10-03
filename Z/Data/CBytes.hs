{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : Z.Data.CBytes
Description : Null-ternimated byte string.
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

-- This module provide 'CBytes' with some useful instances \/ tools for retrieving, storing or processing
-- short byte sequences, such as file path, environment variables, etc.

-}

module Z.Data.CBytes
  ( CBytes(..)
  , pack
  , unpack
  , null , length
  , empty, append, concat, intercalate, intercalateElem
  , toBytes, fromBytes, toText, toTextMaybe, fromText
  , fromCString, fromCString', fromCStringN
  , withCBytesUnsafe, withCBytes, allocCBytesUnsafe, allocCBytes
  -- * exception
  , NullPointerException(..)
  -- re-export
  , CString
  , V.c2w, V.w2c
  ) where

import           Control.DeepSeq
import           Control.Exception (Exception, throwIO)
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Foldable           (foldlM)
import           Data.Hashable           (Hashable(..))
import qualified Data.List               as List
import           Data.Typeable
import           Data.Primitive.PrimArray
import           Data.Word
import           Foreign.C.String
import           GHC.Exts
import           GHC.CString
import           GHC.Ptr
import           GHC.Stack
import           Prelude                 hiding (all, any, appendFile, break,
                                          concat, concatMap, drop, dropWhile,
                                          elem, filter, foldl, foldl1, foldr,
                                          foldr1, getContents, getLine, head,
                                          init, interact, last, length, lines,
                                          map, maximum, minimum, notElem, null,
                                          putStr, putStrLn, readFile, replicate,
                                          reverse, scanl, scanl1, scanr, scanr1,
                                          span, splitAt, tail, take, takeWhile,
                                          unlines, unzip, writeFile, zip,
                                          zipWith)
import           Z.Data.Array
import           Z.Data.Array.UnalignedAccess
import qualified Z.Data.Text           as T
import           Z.Data.Text.UTF8Codec (encodeCharModifiedUTF8, decodeChar)
import qualified Z.Data.Vector.Base    as V
import qualified Z.Data.Vector.Extra   as V
import           Z.Foreign
import           System.IO.Unsafe        (unsafeDupablePerformIO)

-- | A efficient wrapper for short immutable null-terminated byte sequences which can be
-- automatically freed by ghc garbage collector.
--
-- The main use case of this type is to ease the bridging of C FFI APIs, since most
-- of the unix APIs use null-terminated string. On windows you're encouraged to use a
-- compatibility layer like 'WideCharToMultiByte/MultiByteToWideChar' and keep the same
-- interface, e.g. libuv do this when deal with file paths.
--
-- 'CBytes' don't support O(1) slicing, it's not suitable to use it to store large byte
-- chunk, we do save the length info on GHC heap though. If you need advance editing,
-- convert 'CBytes' to \/ from 'V.Bytes' with 'toBytes\/fromBytes' and use vector combinators.
--
-- When textual represatation is needed(conver to 'String', 'T.Text', 'Show' instance, etc.),
-- we assume 'CBytes' using UTF-8 encodings, 'CBytes' can be used with @OverloadedString@,
-- literal encoding is UTF-8 with some modifications: @\NUL@ char is encoded to 'C0 80',
-- and '\xD800' ~ '\xDFFF' is encoded as a three bytes normal utf-8 codepoint.
--
-- Note most of the unix API is not unicode awared though, you may find a `scandir` call
-- return a filename which is not proper encoded in any unicode encoding at all.
-- But still, UTF-8 is recommanded to be used everywhere, and we use UTF-8 assumption in
-- various places.
-- --
data CBytes = CBytes
    { rawCBytes :: {-# UNPACK #-} !(PrimArray Word8)    -- ^ On heap 'PrimArray'
    }                                                   -- there's an invariance that this array's
                                                        -- length is always shrinked to contain content
                                                        -- and \NUL terminator

-- | All exception can be throw by using 'CBytes'.
data CBytesException = NULLTerminatorNotFound CallStack deriving Show
instance Exception CBytesException

instance Show CBytes where
    showsPrec p t = showsPrec p (unpack t)

instance Read CBytes where
    readsPrec p s = [(pack x, r) | (x, r) <- readsPrec p s]

instance NFData CBytes where
    {-# INLINE rnf #-}
    rnf (CBytes _) = ()

instance Eq CBytes where
    {-# INLINE (==) #-}
    CBytes ba == CBytes bb = ba == bb

instance Ord CBytes where
    {-# INLINE compare #-}
    CBytes ba `compare` CBytes bb = ba `compare` bb

instance Semigroup CBytes where
    (<>) = append

instance Monoid CBytes where
    {-# INLINE mempty #-}
    mempty  = empty
    {-# INLINE mappend #-}
    mappend = append
    {-# INLINE mconcat #-}
    mconcat = concat

instance Hashable CBytes where
    hashWithSalt salt (CBytes pa@(PrimArray ba#)) = unsafeDupablePerformIO $ do
        V.c_fnv_hash_ba ba# 0 (sizeofPrimArray pa - 1) salt

instance UnalignedAccess CBytes where
    {-# INLINE unalignedSize #-}
    unalignedSize (CBytes arr ) = sizeofPrimArray arr
    {-# INLINE peekMBA #-}
    peekMBA mba# i = do
        l <- hs_strlen_mba mba# i
        let l' = l+1
        mpa <- newPrimArray l'
        copyMutablePrimArray mpa 0 (MutablePrimArray mba#) i l'
        pa <- unsafeFreezePrimArray mpa
        return (CBytes pa)
    {-# INLINE pokeMBA #-}
    pokeMBA mba# i (CBytes pa) = do
        copyPrimArray (MutablePrimArray mba#) i pa 0 (sizeofPrimArray pa)
    {-# INLINE indexBA #-}
    indexBA ba# i = unsafeDupablePerformIO $ do
        l <- hs_strlen_ba ba# i
        let l' = l+1
        mpa <- newPrimArray l'
        copyPrimArray mpa 0 (PrimArray ba#) i l'
        pa <- unsafeFreezePrimArray mpa
        return (CBytes pa)

append :: CBytes -> CBytes -> CBytes
{-# INLINABLE append #-}
append strA@(CBytes pa) strB@(CBytes pb)
    | lenA == 0 = strB
    | lenB == 0 = strA
    | otherwise = unsafeDupablePerformIO $ do
        mpa <- newPrimArray (lenA+lenB+1)
        copyPrimArray mpa 0    pa 0 lenA
        copyPrimArray mpa lenA pb 0 lenB
        writePrimArray mpa (lenA + lenB) 0     -- the \NUL terminator
        pa' <- unsafeFreezePrimArray mpa
        return (CBytes pa')
  where
    lenA = length strA
    lenB = length strB

-- | 'empty' 'CBytes'
--
-- Passing 'empty' to C FFI is equivalent to passing a @NULL@ pointer.
empty :: CBytes
{-# NOINLINE empty #-}
empty = CBytes (V.singleton 0)

concat :: [CBytes] -> CBytes
{-# INLINABLE concat #-}
concat bss = case pre 0 0 bss of
    (0, _) -> empty
    (1, _) -> let Just b = List.find (not . null) bss in b -- there must be a not empty CBytes
    (_, l) -> runST $ do
        buf <- newPrimArray (l+1)
        copy bss 0 buf
        writePrimArray buf l 0 -- the \NUL terminator
        CBytes <$> unsafeFreezePrimArray buf
  where
    -- pre scan to decide if we really need to copy and calculate total length
    -- we don't accumulate another result list, since it's rare to got empty
    pre :: Int -> Int -> [CBytes] -> (Int, Int)
    pre !nacc !lacc [] = (nacc, lacc)
    pre !nacc !lacc (b:bs)
        | l <= 0 = pre nacc lacc bs
        | otherwise     = pre (nacc+1) (l + lacc) bs
      where !l = length b

    copy :: [CBytes] -> Int -> MutablePrimArray s Word8 -> ST s ()
    copy [] !_ !_       = return ()
    copy (b@(CBytes ba):bs) !i !mba = do
        let l = length b
        when (l /= 0) (copyPrimArray mba i ba 0 l)
        copy bs (i+l) mba

-- | /O(n)/ The 'intercalate' function takes a 'CBytes' and a list of
-- 'CBytes' s and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Note: 'intercalate' will force the entire 'CBytes' list.
--
intercalate :: CBytes -> [CBytes] -> CBytes
{-# INLINE intercalate #-}
intercalate s = concat . List.intersperse s

-- | /O(n)/ An efficient way to join 'CByte' s with a byte.
--
-- @\NUL@ will be encoded as @C0 80@.
intercalateElem :: Word8 -> [CBytes] -> CBytes
{-# INLINABLE intercalateElem #-}
intercalateElem 0 bss = intercalate (CBytes (V.pack [0xC0, 0x80, 0x00])) bss
intercalateElem w8 bss = case len bss 0 of
    0 -> empty
    l -> runST $ do
        buf <- newPrimArray (l+1)
        copy bss 0 buf
        writePrimArray buf l 0 -- the \NUL terminator
        CBytes <$> unsafeFreezePrimArray buf
  where
    len []     !acc = acc
    len [b]    !acc = length b + acc
    len (b:bs) !acc = len bs (acc + length b + 1)
    copy :: [CBytes] -> Int -> MutablePrimArray s Word8 -> ST s ()
    -- bss must not be empty, which is checked by len above
    copy (b@(CBytes ba):bs) !i !mba = do
        let l = length b
        when (l /= 0) (copyPrimArray mba i ba 0 l)
        case bs of
            [] -> return () -- last one
            _  -> do
                let i' = i + l
                writePrimArray mba i' w8
                copy bs (i'+1) mba

instance IsString CBytes where
    {-# INLINE fromString #-}
    fromString = pack

{-# RULES
    "CBytes pack/unpackCString#" forall addr# .
        pack (unpackCString# addr#) = packAddr addr#
 #-}
{-# RULES
    "CBytes pack/unpackCStringUtf8#" forall addr# .
        pack (unpackCStringUtf8# addr#) = packAddr addr#
 #-}

packAddr :: Addr# -> CBytes
packAddr addr0# = go addr0#
  where
    len = 1 + (fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#)
    go addr# = runST $ do
        marr <- newPrimArray len
        copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
        arr <- unsafeFreezePrimArray marr
        return (CBytes arr)

-- | Pack a 'String' into null-terminated 'CBytes'.
--
-- @\NUL@ is encoded as two bytes @C0 80@ , '\xD800' ~ '\xDFFF' is encoded as a three bytes normal UTF-8 codepoint.
pack :: String -> CBytes
{-# INLINE CONLIKE [1] pack #-}
pack s = runST $ do
    mba <- newPrimArray V.defaultInitSize
    (SP2 i mba') <- foldlM go (SP2 0 mba) s
    writePrimArray mba' i 0     -- the \NUL terminator
    shrinkMutablePrimArray mba' (i+1)
    ba <- unsafeFreezePrimArray mba'
    return (CBytes ba)
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: SP2 s -> Char -> ST s (SP2 s)
    go (SP2 i mba) !c     = do
        siz <- getSizeofMutablePrimArray mba
        if i < siz - 4  -- we need at least 5 bytes for safety due to extra '\0' byte
        then do
            i' <- encodeCharModifiedUTF8 mba i c
            return (SP2 i' mba)
        else do
            let !siz' = siz `shiftL` 1
            !mba' <- resizeMutablePrimArray mba siz'
            i' <- encodeCharModifiedUTF8 mba' i c
            return (SP2 i' mba')


data SP2 s = SP2 {-# UNPACK #-}!Int {-# UNPACK #-}!(MutablePrimArray s Word8)

-- | /O(n)/ Convert cbytes to a char list.
--
-- Unpacking is done lazily. i.e. we will retain reference to the array until all element are consumed.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
unpack :: CBytes -> String
{-# INLINE [1] unpack #-}
unpack (CBytes arr) = go 0
  where
    !end = sizeofPrimArray arr - 1
    go !idx
        | idx >= end = []
        | otherwise = let (# c, i #) = decodeChar arr idx in c : go (idx + i)

unpackFB :: CBytes -> (Char -> a -> a) -> a -> a
{-# INLINE [0] unpackFB #-}
unpackFB (CBytes arr) k z = go 0
  where
    !end = sizeofPrimArray arr - 1
    go !idx
        | idx >= end = z
        | otherwise = let (# c, i #) = decodeChar arr idx in c `k` go (idx + i)

{-# RULES
"unpack" [~1] forall t . unpack t = build (\ k z -> unpackFB t k z)
"unpackFB" [1] forall t . unpackFB t (:) [] = unpack t
 #-}

--------------------------------------------------------------------------------

-- Return 'True' if 'CBytes' is empty.
--
null :: CBytes -> Bool
{-# INLINE null #-}
null (CBytes pa) = indexPrimArray pa 0 == 0

-- Return the BTYE length of 'CBytes'.
--
length :: CBytes -> Int
{-# INLINE length #-}
length (CBytes pa) = sizeofPrimArray pa - 1

-- | /O(1)/, convert to 'V.Bytes', which can be processed by vector combinators.
--
-- NOTE: the @\NUL@ ternimator is not included.
toBytes :: CBytes -> V.Bytes
{-# INLINABLE toBytes #-}
toBytes (CBytes arr) = V.PrimVector arr 0 (sizeofPrimArray arr - 1)

-- | /O(n)/, convert from 'V.Bytes'
--
-- Result will be trimmed down to first @\NUL@ byte if there's any, or
-- appends a @\NUL@ byte if there's none.
fromBytes :: V.Bytes -> CBytes
{-# INLINABLE fromBytes #-}
fromBytes v@(V.PrimVector arr s l) = runST (do
    if s == 0 && sizeofPrimArray arr > l && indexPrimArray arr l == 0 -- there's \NUL already
    then return (CBytes arr)
    else do
        let l' = case V.elemIndex 0 v of
                    Just i -> i
                    _ -> l
        mpa <- newPrimArray (l'+1)
        copyPrimArray mpa 0 arr s l'
        writePrimArray mpa l' 0     -- the \NUL terminator
        pa <- unsafeFreezePrimArray mpa
        return (CBytes pa))

-- | /O(n)/, convert to 'T.Text' using UTF8 encoding assumption.
--
-- Throw 'T.InvalidUTF8Exception' in case of invalid codepoint.
toText :: CBytes -> T.Text
{-# INLINABLE toText #-}
toText = T.validate . toBytes

-- | /O(n)/, convert to 'T.Text' using UTF8 encoding assumption.
--
-- Return 'Nothing' in case of invalid codepoint.
toTextMaybe :: CBytes -> Maybe T.Text
{-# INLINABLE toTextMaybe #-}
toTextMaybe = T.validateMaybe . toBytes

-- | /O(n)/, convert from 'T.Text',
--
-- Result will be trimmed down to first @\NUL@ byte if there's any, or
-- appends a @\NUL@ byte if there's none.
fromText :: T.Text -> CBytes
{-# INLINABLE fromText #-}
fromText = fromBytes . T.getUTF8Bytes

--------------------------------------------------------------------------------

-- | Copy a 'CString' type into a 'CBytes', return 'empty' if the pointer is NULL.
--
--  After copying you're free to free the 'CString' 's memory.
fromCString :: CString -> IO CBytes
{-# INLINABLE fromCString #-}
fromCString cstring = do
    if cstring == nullPtr
    then return empty
    else do
        len <- fromIntegral <$> c_strlen_ptr cstring
        let len' = len + 1
        mpa <- newPrimArray len'
        copyPtrToMutablePrimArray mpa 0 (castPtr cstring) len'
        pa <- unsafeFreezePrimArray mpa
        return (CBytes pa)

-- | Same with 'fromCString', but throw 'NullPointerException' when meet a null pointer.
--
fromCString' :: HasCallStack => CString -> IO (Maybe CBytes)
{-# INLINABLE fromCString' #-}
fromCString' cstring =
    if cstring == nullPtr
    then throwIO (NullPointerException callStack)
    else do
        len <- fromIntegral <$> c_strlen_ptr cstring
        let len' = len + 1
        mpa <- newPrimArray len'
        copyPtrToMutablePrimArray mpa 0 (castPtr cstring) len'
        pa <- unsafeFreezePrimArray mpa
        return (Just (CBytes pa))

-- | Same with 'fromCString', but only take at most N bytes.
--
-- Result will be trimmed down to first @\NUL@ byte if there's any, or
-- appends a @\NUL@ byte if there's none.
--
fromCStringN :: CString -> Int -> IO CBytes
{-# INLINABLE fromCStringN #-}
fromCStringN cstring len0 = do
    if cstring == nullPtr || len0 == 0
    then return empty
    else do
        len1 <- fromIntegral <$> c_strlen_ptr cstring
        let len = min len0 len1
        mpa <- newPrimArray (len+1)
        copyPtrToMutablePrimArray mpa 0 (castPtr cstring) len
        writePrimArray mpa len 0     -- the \NUL terminator
        pa <- unsafeFreezePrimArray mpa
        return (CBytes pa)

data NullPointerException = NullPointerException CallStack deriving (Show, Typeable)
instance Exception NullPointerException

-- | Pass 'CBytes' to foreign function as a @const char*@.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
withCBytesUnsafe :: CBytes -> (BA# Word8 -> IO a) -> IO a
{-# INLINABLE withCBytesUnsafe #-}
withCBytesUnsafe (CBytes pa) f = withPrimArrayUnsafe pa (\ p _ -> f p)

-- | Pass 'CBytes' to foreign function as a @const char*@.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withCBytes :: CBytes -> (Ptr Word8 -> IO a) -> IO a
{-# INLINABLE withCBytes #-}
withCBytes (CBytes pa) f = withPrimArraySafe pa (\ p _ -> f p)

-- | Create a 'CBytes' with IO action.
--
-- If (<=0) capacity is provided, a pointer pointing to @\NUL@ is passed to initialize function
-- and 'empty' will be returned. Which may cause troubles for some FFI functions.
-- Other than that, User have to make sure a @\NUL@ ternimated
-- string will be written, otherwise a 'NULLTerminatorNotFound' will be thrown.
allocCBytesUnsafe :: HasCallStack
                  => Int  -- ^ capacity n, including the @\NUL@ terminator
                  -> (MBA# Word8 -> IO a)  -- ^ initialization function,
                  -> IO (CBytes, a)
{-# INLINABLE allocCBytesUnsafe #-}
allocCBytesUnsafe n fill | n <= 0 = withPrimUnsafe (0::Word8) fill >>=
                                        \ (_, b) -> return (empty, b)
                         | otherwise = do
    mba@(MutablePrimArray mba#) <- newPrimArray n :: IO (MutablePrimArray RealWorld Word8)
    a <- fill mba#
    l <- fromIntegral <$> (c_strlen_mba mba#)
    when (l+1>n) (throwIO (NULLTerminatorNotFound callStack))
    shrinkMutablePrimArray mba (l+1)
    bs <- unsafeFreezePrimArray mba
    return (CBytes bs, a)



-- | Create a 'CBytes' with IO action.
--
-- If (<=0) capacity is provided, a 'nullPtr' is passed to initialize function and
-- 'empty' will be returned. Other than that, User have to make sure a @\NUL@ ternimated
-- string will be written, otherwise a 'NULLTerminatorNotFound' will be thrown.
allocCBytes :: HasCallStack
            => Int  -- ^ capacity n, including the @\NUL@ terminator
            -> (CString -> IO a)  -- ^ initialization function,
            -> IO (CBytes, a)
{-# INLINABLE allocCBytes #-}
allocCBytes n fill | n <= 0 = fill nullPtr >>= \ a -> return (empty, a)
                   | otherwise = do
    mba <- newPinnedPrimArray n :: IO (MutablePrimArray RealWorld Word8)
    a <- withMutablePrimArrayContents mba (fill . castPtr)
    l <- fromIntegral <$> withMutablePrimArrayContents mba (c_strlen_ptr . castPtr)
    when (l+1>n) (throwIO (NULLTerminatorNotFound callStack))
    shrinkMutablePrimArray mba (l+1)
    bs <- unsafeFreezePrimArray mba
    return (CBytes bs, a)

--------------------------------------------------------------------------------

c_strlen_ptr :: CString -> IO CSize
{-# INLINE c_strlen_ptr #-}
c_strlen_ptr (Ptr a#) = V.c_strlen a#

foreign import ccall unsafe "string.h strlen" c_strlen_mba :: MBA# Word8 -> IO CSize
foreign import ccall unsafe "string.h hs_strlen" hs_strlen_mba :: MBA# Word8 -> Int -> IO Int
foreign import ccall unsafe "string.h hs_strlen" hs_strlen_ba :: BA# Word8 -> Int -> IO Int
