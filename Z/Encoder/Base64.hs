{-|
Module      : Z.Data.Vector.Base64
Description : Base64 codec for bytes.
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides base64 encoding & decoding tools, as well as 'Base64Bytes' newtype with base64 textual instances.

-}

module Z.Data.Vector.Base64
  (-- * Encoding & Decoding functions
    base64
  , base64EncodeLength
  , base64Text
  , base64EncodeEncoder
  , decodeBase64
  , decodeBase64'
  , base64DecodeLength
  , Base64DecodeException(..)
  -- * Internal C FFIs
  ,  hs_base64_encode, hs_base64_decode
  ) where

import           Control.Exception
import           Data.Word
import           Data.Bits                      (unsafeShiftL, unsafeShiftR, (.&.))
import           GHC.Stack
import           GHC.Exts
import           GHC.IO
import qualified Z.Data.Vector.Base         as V
import qualified Z.Data.Encoder.Base        as B
import qualified Z.Data.Text.Base           as T
import           Z.Data.Foreign

-- | Encode 'V.Bytes' using base64 encoding.
base64 :: V.Bytes -> V.Bytes
{-# INLINABLE base64 #-}
base64 v = fst . unsafeDupablePerformIO $ do
    withPrimVectorUnsafe v $ \ ba s e ->
        allocPrimVectorUnsafe base64Len $ \ mba ->
            z_base64_encode mba 0 ba s len
  where
    len = e-s 
    base64Len = base64EncodeLength len

-- | Return the encoded length of a given input length, always a multipler of 4.
base64EncodeLength :: Int -> Int
{-# INLINE base64EncodeLength #-}
base64EncodeLength n = ((n+2) `quot` 3) `unsafeShiftL` 2

-- | 'B.Encoder' version of 'base64'.
encodeBase64 :: V.Bytes -> B.Encoder ()
{-# INLINE encodeBase64 #-}
encodeBase64 (V.PrimVector arr s e) =
    B.writeN base64Len (\ (MutablePrimArray mba#) i -> do
        withPrimArrayUnsafe arr $ \ parr _ ->
            unsafeIOToST (z_base64_encode (unsafeCoerce# mba#) i parr s len))
  where
    len = e-s 
    base64Len = base64EncodeLength len

-- | Text version of 'base64'.
base64Text :: V.Bytes -> T.Text
{-# INLINABLE base64Text #-}
base64Text = T.Text . base64

-- | Decode a base64 encoding string, return Nothing on illegal bytes or incomplete input.
decodeBase64 :: V.Bytes -> Maybe V.Bytes
{-# INLINABLE decodeBase64 #-}
decodeBase64 ba
    | inputLen == 0 = Just V.empty
    | decodeLen == -1 = Nothing
    | otherwise = unsafeDupablePerformIO $ do
        (arr, r) <- withPrimVectorUnsafe ba $ \ ba# s _ ->
            allocPrimArrayUnsafe decodeLen $ \ buf# ->
                z_base64_decode buf# ba# s inputLen
        if r == 0
        then return Nothing
        else return (Just (V.PrimVector arr 0 r))
  where
    inputLen = V.length ba
    decodeLen = base64DecodeLength inputLen

-- | Exception during base64 decoding.
data Base64DecodeException = IllegalBase64Bytes V.Bytes CallStack deriving Show
instance Exception Base64DecodeException

-- | Decode a base64 encoding string, throw 'Base64DecodeException' on error.
decodeBase64' :: HasCallStack => V.Bytes -> V.Bytes
{-# INLINABLE decodeBase64' #-}
decodeBase64' ba = case decodeBase64 ba of
    Just r -> r
    _ -> throw (IllegalBase64Bytes ba callStack)

-- | Return the upper bound of decoded length of a given input length
-- , return -1 if illegal(not a multipler of 4).
base64DecodeLength :: Int -> Int
{-# INLINE base64DecodeLength #-}
base64DecodeLength n | n .&. 3 == 1 = -1
                     | otherwise = (n `unsafeShiftR` 2) * 3 + 2

--------------------------------------------------------------------------------

foreign import ccall unsafe z_base64_encode :: MBA# Word8 -> Int -> BA# Word8 -> Int -> Int -> IO ()
foreign import ccall unsafe z_base64_decode :: MBA# Word8 -> BA# Word8 -> Int -> Int -> IO Int
