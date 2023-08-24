module Z.Data.Utils.FFI where

import GHC.Exts
import GHC.Word
import GHC.Int
import Foreign.C
import Z.Data.Utils.UTF8Rewind

--------------------------------------------------------------------------------
-- z_bytes.c

foreign import ccall unsafe "string.h strlen"
    c_strlen :: Addr# -> IO CSize

foreign import ccall unsafe
    z_fnv_hash_addr :: Addr# -> Int -> Int -> IO Int

foreign import ccall unsafe
    z_fnv_hash :: ByteArray# -> Int -> Int -> Int -> IO Int

-- HsInt z_memchr(uint8_t *a, HsInt aoff, uint8_t b, HsInt n);
foreign import ccall unsafe
    z_memchr :: ByteArray# -> Int -> Word8 -> Int -> Int

-- HsInt hs_memrchr(uint8_t *a, HsInt aoff, uint8_t b, HsInt n);
foreign import ccall unsafe
    z_memrchr :: ByteArray# -> Int -> Word8 -> Int -> Int

foreign import ccall unsafe
    z_count :: ByteArray# -> Int -> Int -> Word8 -> IO Int

foreign import ccall unsafe
    z_intersperse :: MutableByteArray# RealWorld -> ByteArray# -> Int -> Int -> Word8 -> IO ()

--------------------------------------------------------------------------------
-- z_text.c

foreign import ccall unsafe
    z_escape_json_string_length :: ByteArray# -> Int -> Int -> Int
foreign import ccall unsafe
    z_escape_json_string :: ByteArray# -> Int -> Int -> MutableByteArray# RealWorld -> Int -> IO Int

foreign import ccall unsafe
    z_utf8_validate :: ByteArray# -> Int# -> Int# -> Int
foreign import ccall unsafe
    z_utf8_validate_addr :: Addr# -> Int -> IO Int
foreign import ccall unsafe
    z_ascii_validate :: ByteArray# -> Int# -> Int# -> Int
foreign import ccall unsafe
    z_ascii_validate_addr :: Addr# -> Int -> IO Int

foreign import ccall unsafe "z_text_width.c z_wcwidth" z_wcwidth :: Int32 -> Int

-- functions below will return error if the source ByteArray# is empty
foreign import ccall unsafe 
    z_utf8_iscategory :: ByteArray# -> Int# -> Int# -> Category -> Int
foreign import ccall unsafe 
    z_utf8_isnormalized :: ByteArray# -> Int# -> Int# -> CSize -> Int
foreign import ccall unsafe
    z_utf8_normalize :: ByteArray# -> Int# -> Int# -> MutableByteArray# RealWorld -> Int# -> CSize -> IO ()
foreign import ccall unsafe 
    z_utf8_normalize_length :: ByteArray# -> Int# -> Int# -> CSize -> Int
foreign import ccall unsafe 
    z_utf8_casefold :: ByteArray# -> Int# -> Int# -> MutableByteArray# RealWorld -> Int# -> Locale -> IO ()
foreign import ccall unsafe
    z_utf8_casefold_length :: ByteArray# -> Int# -> Int# -> Locale -> Int
foreign import ccall unsafe
    z_utf8_tolower :: ByteArray# -> Int# -> Int# -> MutableByteArray# RealWorld -> Int# -> Locale -> IO ()
foreign import ccall unsafe 
    z_utf8_tolower_length :: ByteArray# -> Int# -> Int# -> Locale -> Int
foreign import ccall unsafe
    z_utf8_toupper :: ByteArray# -> Int# -> Int# -> MutableByteArray# RealWorld -> Int# -> Locale -> IO ()
foreign import ccall unsafe 
    z_utf8_toupper_length :: ByteArray# -> Int# -> Int# -> Locale -> Int
foreign import ccall unsafe
    z_utf8_totitle :: ByteArray# -> Int# -> Int# -> MutableByteArray# RealWorld -> Int# -> Locale -> IO ()
foreign import ccall unsafe 
    z_utf8_totitle_length :: ByteArray# -> Int# -> Int# -> Locale -> Int

