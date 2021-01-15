{-|
Module      : Z.Data.CBytes
Description : Null-ternimated byte string.
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides 'CBytes' with some useful instances \/ tools for retrieving, storing or processing
short byte sequences, such as file path, environment variables, etc.

-}

module Z.Data.CBytes
  (  -- * The CBytes type
    CBytes(CB)
  , rawPrimArray, fromPrimArray
  , toBytes, fromBytes, toText, toTextMaybe, fromText, toBuilder, buildCBytes
  , pack
  , unpack
  , null, length
  , empty, singleton, append, concat, intercalate, intercalateElem
  , fromCString, fromCStringN, fromStdString
  , withCBytesUnsafe, withCBytes, allocCBytesUnsafe, allocCBytes
  , withCBytesListUnsafe, withCBytesList
  , pokeMBACBytes, peekMBACBytes, indexBACBytes
  -- * re-export
  , CString
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Foldable              (foldlM)
import           Data.Hashable              (Hashable(..))
import qualified Data.List                  as List
import           Data.Primitive.PrimArray
import           Data.Word
import           Foreign.C.String
import           GHC.Exts
import           GHC.CString
import           GHC.Ptr
import           GHC.Stack
import           Prelude                    hiding (all, any, appendFile, break,
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
import qualified Z.Data.Builder             as B
import qualified Z.Data.Text                as T
import qualified Z.Data.Text.Print          as T
import qualified Z.Data.Text.UTF8Codec      as T
import qualified Z.Data.JSON.Base           as JSON
import           Z.Data.JSON.Base           ((<?>))
import           Z.Data.Text.UTF8Codec      (encodeCharModifiedUTF8, decodeChar)
import qualified Z.Data.Vector.Base         as V
import           Z.Foreign                  hiding (fromStdString)
import           System.IO.Unsafe           (unsafeDupablePerformIO)
import           Test.QuickCheck.Arbitrary  (Arbitrary(..), CoArbitrary(..))
import           Text.Read                 (Read(..))

-- | A efficient wrapper for short immutable null-terminated byte sequences which can be
-- automatically freed by ghc garbage collector.
--
-- The main use case of this type is to ease the bridging of C FFI APIs, since most
-- of the unix APIs use null-terminated string. On windows you're encouraged to use a
-- compatibility layer like 'WideCharToMultiByte/MultiByteToWideChar' and keep the same
-- interface, e.g. libuv do this when deal with file paths.
--
-- 'CBytes' don't support O(1) slicing, it's not suitable to use it to store large byte
-- chunk, If you need advance editing, convert 'CBytes' to\/from 'V.Bytes' with 'CB' pattern or
-- 'toBytes' \/ 'fromBytes', then use vector combinators.
--
-- When textual represatation is needed e.g. converting to 'String', 'T.Text', 'Show' instance, etc.,
-- we assume 'CBytes' using UTF-8 encodings, 'CBytes' can be used with @OverloadedString@,
-- literal encoding is UTF-8 with some modifications: @\\NUL@ is encoded to 'C0 80',
-- and @\\xD800@ ~ @\\xDFFF@ is encoded as a three bytes normal utf-8 codepoint.
--
-- Note most of the unix API is not unicode awared though, you may find a `scandir` call
-- return a filename which is not proper encoded in any unicode encoding at all.
-- But still, UTF-8 is recommanded to be used when text represatation is needed.
--
newtype CBytes = CBytes
    {
        -- | Convert to a @\\NUL@ terminated 'PrimArray',
        --
        -- There's an invariance that this array never contains extra @\\NUL@ except terminator.
        rawPrimArray :: PrimArray Word8
    }

-- | Constuctor a 'CBytes' from arbitrary array, result will be trimmed down to first @\\NUL@ byte if there's any.
fromPrimArray :: PrimArray Word8 -> CBytes
{-# INLINE fromPrimArray #-}
fromPrimArray arr = runST (do
    let l = case V.elemIndex 0 arr of
            Just i -> i
            _ -> sizeofPrimArray arr
    mpa <- newPrimArray (l+1)
    copyPrimArray mpa 0 arr 0 l
    -- write \\NUL terminator
    writePrimArray mpa l 0
    pa <- unsafeFreezePrimArray mpa
    return (CBytes pa))

-- | Use this pattern to match or construct 'CBytes', result will be trimmed down to first @\\NUL@ byte if there's any.
pattern CB :: V.Bytes -> CBytes
{-# COMPLETE CB #-}
pattern CB bs <- (toBytes -> bs) where
    CB bs = fromBytes bs

instance Show CBytes where
    showsPrec p t = showsPrec p (unpack t)

instance Read CBytes where
    readPrec = pack <$> readPrec

instance NFData CBytes where
    {-# INLINE rnf #-}
    rnf (CBytes _) = ()

instance Eq CBytes where
    {-# INLINE (==) #-}
    -- \\NUL does not affect equality
    CBytes ba == CBytes bb = ba == bb

instance Ord CBytes where
    {-# INLINE compare #-}
    -- \\NUL does not affect ordering
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

instance Arbitrary CBytes where
    arbitrary = pack <$> arbitrary
    shrink a = pack <$> shrink (unpack a)

instance CoArbitrary CBytes where
    coarbitrary = coarbitrary . unpack

-- | Poke 'CBytes' until a \\NUL terminator(or to the end of the array if there's none).
peekMBACBytes :: MBA# Word8 -> Int -> IO CBytes
{-# INLINE peekMBACBytes #-}
peekMBACBytes mba# i = do
    b <- getSizeofMutableByteArray (MutableByteArray mba#)
    let rest = b-i
    l <- c_memchr mba# i 0 rest
    let l' = if l == -1 then rest else l
    mpa <- newPrimArray (l'+1)
    copyMutablePrimArray mpa 0 (MutablePrimArray mba#) i l'
    -- write \\NUL terminator
    writePrimArray mpa l' 0
    pa <- unsafeFreezePrimArray mpa
    return (CBytes pa)

-- | Poke 'CBytes' with \\NUL terminator.
pokeMBACBytes :: MBA# Word8 -> Int -> CBytes -> IO ()
{-# INLINE pokeMBACBytes #-}
pokeMBACBytes mba# i (CBytes pa) = do
        let l = sizeofPrimArray pa
        copyPrimArray (MutablePrimArray mba# :: MutablePrimArray RealWorld Word8) i pa 0 l

indexBACBytes :: BA# Word8 -> Int -> CBytes
{-# INLINE indexBACBytes #-}
indexBACBytes ba# i = runST (do
    let b = sizeofByteArray (ByteArray ba#)
        rest = b-i
        l = V.c_memchr ba# i 0 rest
        l' = if l == -1 then rest else l
    mpa <- newPrimArray (l'+1)
    copyPrimArray mpa 0 (PrimArray ba#) i l'
    writePrimArray mpa l' 0
    pa <- unsafeFreezePrimArray mpa
    return (CBytes pa))

-- | This instance provide UTF8 guarantee, illegal codepoints will be written as 'T.replacementChar's.
--
-- Escaping rule is same with 'String'.
instance T.Print CBytes where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = T.stringUTF8 . show . unpack

-- | JSON instances check if 'CBytes' is proper UTF8 encoded,
-- if it is, decode/encode it as 'T.Text', otherwise as 'V.Bytes'.
--
-- @
-- > encodeText ("hello" :: CBytes)
-- "\"hello\""
-- > encodeText ("hello\\NUL" :: CBytes)     -- @\\NUL@ is encoded as C0 80
-- "[104,101,108,108,111,192,128]"
-- @
instance JSON.JSON CBytes where
    {-# INLINE fromValue #-}
    fromValue value =
        case value of
            JSON.String t ->
                return (fromText t)
            JSON.Array arr ->
                fromBytes <$> V.traverseWithIndex
                    (\ k v -> JSON.fromValue v <?> JSON.Index k) arr
            _ -> JSON.fail'
                    "converting Z.Data.CBytes.CBytes failed, expected array or string"
    {-# INLINE toValue #-}
    toValue cbytes = case toTextMaybe cbytes of
        Just t -> JSON.toValue t
        Nothing -> JSON.toValue (toBytes cbytes)
    {-# INLINE encodeJSON #-}
    encodeJSON cbytes = case toTextMaybe cbytes of
        Just t -> JSON.encodeJSON t
        Nothing -> B.square . JSON.commaSepVec . toBytes $ cbytes

-- | Concatenate two 'CBytes'.
append :: CBytes -> CBytes -> CBytes
{-# INLINABLE append #-}
append strA@(CBytes pa) strB@(CBytes pb)
    | lenA == 0 = strB
    | lenB == 0 = strA
    | otherwise = unsafeDupablePerformIO $ do
        mpa <- newPrimArray (lenA+lenB+1)
        copyPrimArray mpa 0    pa 0 lenA
        copyPrimArray mpa lenA pb 0 lenB
        writePrimArray mpa (lenA + lenB) 0     -- the \\NUL terminator
        pa' <- unsafeFreezePrimArray mpa
        return (CBytes pa')
  where
    lenA = length strA
    lenB = length strB

-- | Empty 'CBytes'
empty :: CBytes
{-# NOINLINE empty #-}
empty = CBytes (V.singleton 0)

-- | Singleton 'CBytes'.
singleton :: Word8 -> CBytes
{-# INLINE singleton #-}
singleton w = runST (do
    buf <- newPrimArray 2
    writePrimArray buf 0 w
    writePrimArray buf 1 0
    pa <- unsafeFreezePrimArray buf
    return (CBytes pa))

-- | /O(n)/ Concatenate a list of 'CBytes'.
--
concat :: [CBytes] -> CBytes
{-# INLINABLE concat #-}
concat bss = case pre 0 0 bss of
    (0, _) -> empty
    (1, _) -> let Just b = List.find (not . null) bss in b -- there must be a not empty CBytes
    (_, l) -> runST $ do
        buf <- newPrimArray (l+1)
        copy bss 0 buf
        writePrimArray buf l 0 -- the \\NUL terminator
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
-- Intercalate bytes list with @\\NUL@ will effectively leave the first bytes in the list.
intercalateElem :: Word8 -> [CBytes] -> CBytes
{-# INLINABLE intercalateElem #-}
intercalateElem 0 [] = empty
intercalateElem 0 (bs:_) = bs
intercalateElem w8 bss = case len bss 0 of
    0 -> empty
    l -> runST $ do
        buf <- newPrimArray (l+1)
        copy bss 0 buf
        writePrimArray buf l 0 -- the \\NUL terminator
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
{-# INLINE packAddr #-}
packAddr addr0# = go addr0#
  where
    len = (fromIntegral . unsafeDupablePerformIO $ V.c_strlen addr0#) + 1
    go addr# = runST $ do
        marr <- newPrimArray len
        copyPtrToMutablePrimArray marr 0 (Ptr addr#) len
        arr <- unsafeFreezePrimArray marr
        return (CBytes arr)

-- | Pack a 'String' into 'CBytes'.
--
-- @\\NUL@ is encoded as two bytes @C0 80@ , @\\xD800@ ~ @\\xDFFF@ is encoded as a three bytes normal UTF-8 codepoint.
pack :: String -> CBytes
{-# INLINE CONLIKE [1] pack #-}
pack s = runST $ do
    mba <- newPrimArray V.defaultInitSize
    (SP2 i mba') <- foldlM go (SP2 0 mba) s
    writePrimArray mba' i 0     -- the \\NUL terminator
    shrinkMutablePrimArray mba' (i+1)
    ba <- unsafeFreezePrimArray mba'
    return (CBytes ba)
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: SP2 s -> Char -> ST s (SP2 s)
    go (SP2 i mba) !c     = do
        siz <- getSizeofMutablePrimArray mba
        if i < siz - 4  -- we need at least 4 bytes for safety due to extra '\0' byte
        then do
            i' <- encodeCharModifiedUTF8 mba i c
            return (SP2 i' mba)
        else do
            let !siz' = siz `shiftL` 1
            !mba' <- resizeMutablePrimArray mba siz'
            i' <- encodeCharModifiedUTF8 mba' i c
            return (SP2 i' mba')


data SP2 s = SP2 {-# UNPACK #-}!Int {-# UNPACK #-}!(MutablePrimArray s Word8)

-- | /O(n)/ Convert cbytes to a char list using UTF8 encoding assumption.
--
-- This function is much tolerant than 'toText', it simply decoding codepoints using UTF8 'decodeChar'
-- without checking errors such as overlong or invalid range.
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
        | idx + T.decodeCharLen arr idx > end = [T.replacementChar]
        | otherwise = let (# c, i #) = decodeChar arr idx in c : go (idx + i)

unpackFB :: CBytes -> (Char -> a -> a) -> a -> a
{-# INLINE [0] unpackFB #-}
unpackFB (CBytes arr) k z = go 0
  where
    !end = sizeofPrimArray arr - 1
    go !idx
        | idx >= end = z
        | idx + T.decodeCharLen arr idx > end = T.replacementChar `k` z
        | otherwise = let (# c, i #) = decodeChar arr idx in c `k` go (idx + i)

{-# RULES
"unpack" [~1] forall t . unpack t = build (\ k z -> unpackFB t k z)
"unpackFB" [1] forall t . unpackFB t (:) [] = unpack t
 #-}

--------------------------------------------------------------------------------

-- | Return 'True' if 'CBytes' is empty.
--
null :: CBytes -> Bool
{-# INLINE null #-}
null (CBytes pa) = indexPrimArray pa 0 == 0

-- | /O(1)/, Return the BTYE length of 'CBytes'.
--
length :: CBytes -> Int
{-# INLINE length #-}
length (CBytes pa) = sizeofPrimArray pa - 1

-- | /O(1)/, convert to 'V.Bytes', which can be processed by vector combinators.
toBytes :: CBytes -> V.Bytes
{-# INLINABLE toBytes #-}
toBytes (CBytes arr) = V.PrimVector arr 0 (sizeofPrimArray arr - 1)

-- | /O(n)/, convert from 'V.Bytes'
--
-- Result will be trimmed down to first @\\NUL@ byte if there's any.
fromBytes :: V.Bytes -> CBytes
{-# INLINABLE fromBytes #-}
fromBytes v@(V.PrimVector arr s l)
        -- already a \\NUL terminated bytes
    | s == 0 && sizeofPrimArray arr == (l+1) && indexPrimArray arr l == 0 =
        CBytes arr
    | otherwise = runST (do
        let l' = case V.elemIndex 0 v of
                Just i -> i
                _ -> l
        mpa <- newPrimArray (l'+1)
        copyPrimArray mpa 0 arr s l'
        writePrimArray mpa l' 0      -- the \\NUL terminator
        pa <- unsafeFreezePrimArray mpa
        return (CBytes pa))

-- | /O(n)/, convert to 'T.Text' using UTF8 encoding assumption.
--
-- Throw 'T.InvalidUTF8Exception' in case of invalid codepoint.
toText :: HasCallStack => CBytes -> T.Text
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
-- Result will be trimmed down to first @\\NUL@ byte if there's any.
fromText :: T.Text -> CBytes
{-# INLINABLE fromText #-}
fromText = fromBytes . T.getUTF8Bytes

-- | Write 'CBytes' \'s byte sequence to buffer.
--
-- This function is different from 'T.Print' instance in that it directly write byte sequence without
-- checking if it's UTF8 encoded.
toBuilder :: CBytes -> B.Builder ()
toBuilder = B.bytes . toBytes

-- | Build a 'CBytes' with builder, result will be trimmed down to first @\\NUL@ byte if there's any.
buildCBytes :: B.Builder a -> CBytes
buildCBytes = fromBytes . B.build

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

-- | Same with 'fromCString', but only take at most N bytes.
--
-- Result will be trimmed down to first @\\NUL@ byte if there's any.
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
        writePrimArray mpa len 0     -- the \\NUL terminator
        pa <- unsafeFreezePrimArray mpa
        return (CBytes pa)

-- | Pass 'CBytes' to foreign function as a @const char*@.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
withCBytesUnsafe :: CBytes -> (BA# Word8 -> IO a) -> IO a
{-# INLINABLE withCBytesUnsafe #-}
withCBytesUnsafe (CBytes pa) f = withPrimArrayUnsafe pa (\ p _ -> f p)

-- | Pass 'CBytes' list to foreign function as a @StgArrBytes**@.
--
-- Enable 'UnliftedFFITypes' extension in your haskell code, use @StgArrBytes**@(>=8.10)
-- or @StgMutArrPtrs*@(<8.10) pointer type and @HsInt@
-- to marshall @BAArray#@ and @Int@ arguments on C side, check the example with 'BAArray#'.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
withCBytesListUnsafe :: [CBytes] -> (BAArray# Word8 -> Int -> IO a) -> IO a
{-# INLINABLE withCBytesListUnsafe #-}
withCBytesListUnsafe pas = withPrimArrayListUnsafe (List.map rawPrimArray pas)

-- | Pass 'CBytes' to foreign function as a @const char*@.
--
-- Don't pass a forever loop to this function, see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withCBytes :: CBytes -> (Ptr Word8 -> IO a) -> IO a
{-# INLINABLE withCBytes #-}
withCBytes (CBytes pa) f = withPrimArraySafe pa (\ p _ -> f p)

-- | Pass 'CBytes' list to foreign function as a @const char**@.
--
-- Check "Z.Foreign" module for more detail on how to marshall params in C side.
withCBytesList :: [CBytes] -> (Ptr (Ptr Word8) -> Int -> IO a) -> IO a
{-# INLINABLE withCBytesList #-}
withCBytesList pas = withPrimArrayListSafe (List.map rawPrimArray pas)

-- | Create a 'CBytes' with IO action.
--
-- If (<=0) capacity is provided, a pointer pointing to @\\NUL@ is passed to initialize function
-- and 'empty' will be returned. This behavior is different from 'allocCBytes', which may cause
-- trouble for some FFI functions.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
allocCBytesUnsafe :: HasCallStack
                  => Int                   -- ^ capacity n(including the @\\NUL@ terminator)
                  -> (MBA# Word8 -> IO a)  -- ^ initialization function,
                  -> IO (CBytes, a)
{-# INLINABLE allocCBytesUnsafe #-}
allocCBytesUnsafe n fill | n <= 0 = withPrimUnsafe (0::Word8) fill >>=
                                        \ (_, b) -> return (empty, b)
                         | otherwise = do
    mba@(MutablePrimArray mba#) <- newPrimArray n :: IO (MutablePrimArray RealWorld Word8)
    a <- fill mba#
    l <- fromIntegral <$> (c_memchr mba# 0 0 n)
    let l' = if l == -1 then (n-1) else l
    shrinkMutablePrimArray mba (l'+1)
    writePrimArray mba l' 0
    bs <- unsafeFreezePrimArray mba
    return (CBytes bs, a)


-- | Create a 'CBytes' with IO action.
--
-- If (<=0) capacity is provided, a 'nullPtr' is passed to initialize function and
-- 'empty' will be returned. Other than that, User have to make sure a @\\NUL@ ternimated
-- string will be written.
allocCBytes :: HasCallStack
            => Int                -- ^ capacity n(including the @\\NUL@ terminator)
            -> (CString -> IO a)  -- ^ initialization function,
            -> IO (CBytes, a)
{-# INLINABLE allocCBytes #-}
allocCBytes n fill | n <= 0 = fill nullPtr >>= \ a -> return (empty, a)
                   | otherwise = do
    mba@(MutablePrimArray mba#) <- newPinnedPrimArray n :: IO (MutablePrimArray RealWorld Word8)
    a <- withMutablePrimArrayContents mba (fill . castPtr)
    l <- fromIntegral <$> (c_memchr mba# 0 0 n)
    let l' = if l == -1 then (n-1) else l
    shrinkMutablePrimArray mba (l'+1)
    writePrimArray mba l' 0
    bs <- unsafeFreezePrimArray mba
    return (CBytes bs, a)

-- | Run FFI in bracket and marshall @std::string*@ result into 'CBytes',
-- memory pointed by @std::string*@ will be @delete@ ed.
fromStdString :: IO (Ptr StdString) -> IO CBytes
fromStdString f = bracket f hs_delete_std_string
    (\ q -> do
        siz <- hs_std_string_size q
        let !siz' = siz + 1
        (bs,_) <- allocPrimArrayUnsafe siz' (hs_copy_std_string q siz')
        return (CBytes bs))

--------------------------------------------------------------------------------

c_strlen_ptr :: CString -> IO CSize
{-# INLINE c_strlen_ptr #-}
c_strlen_ptr (Ptr a#) = V.c_strlen a#

-- HsInt hs_memchr(uint8_t *a, HsInt aoff, uint8_t b, HsInt n);
foreign import ccall unsafe "hs_memchr" c_memchr :: MBA# Word8 -> Int -> Word8 -> Int -> IO Int
