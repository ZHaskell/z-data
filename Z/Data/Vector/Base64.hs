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
  ( -- * The Base64Bytes type
    Base64Bytes(..)
  -- * Encoding & Decoding functions
  , base64Encode
  , base64EncodeLength
  , base64EncodeText
  , base64EncodeBuilder
  , base64Decode
  , base64Decode'
  , base64DecodeLength
  , Base64DecodeException(..)
  -- * Base64 helpers
  ,  hs_base64_encode, hs_base64_decode
  ) where

import           Control.Exception
import           Data.Word
import           Data.Bits                      (unsafeShiftL, unsafeShiftR, (.&.))
import           Data.Hashable                  (Hashable(..))
import           GHC.Stack
import           System.IO.Unsafe
import qualified Z.Data.Vector.Base         as V
import qualified Z.Data.Builder.Base        as B
import qualified Z.Data.Text.Base           as T
import qualified Z.Data.Text.ShowT          as T
import qualified Z.Data.JSON                as JSON
import           Z.Foreign

-- | New type wrapper for 'V.Bytes' with base64 encoding Show\/JSON instances.
newtype Base64Bytes = Base64Bytes { unBase64Bytes :: V.Bytes }
    deriving (Eq, Ord)
    deriving newtype (Monoid, Semigroup, Hashable)

instance Show Base64Bytes where
    show (Base64Bytes bs) = T.unpack $ base64EncodeText bs

instance T.ShowT Base64Bytes where
    toTextBuilder _ (Base64Bytes bs) = T.unsafeFromBuilder $ B.quotes (base64EncodeBuilder bs)

instance JSON.FromValue Base64Bytes where
    {-# INLINE fromValue #-}
    fromValue = JSON.withText "Z.Data.Text.Base64Bytes" $ \ t ->
        case base64Decode (T.getUTF8Bytes t) of
            Just bs -> return (Base64Bytes bs)
            Nothing -> JSON.fail' "illegal base64 encoding bytes"

instance JSON.ToValue Base64Bytes where
    {-# INLINE toValue #-}
    toValue (Base64Bytes bs) = JSON.String (base64EncodeText bs)

instance JSON.EncodeJSON Base64Bytes where
    {-# INLINE encodeJSON #-}
    encodeJSON (Base64Bytes bs) = base64EncodeBuilder bs

-- | Encode 'V.Bytes' using base64 encoding.
base64Encode :: V.Bytes -> V.Bytes
base64Encode (V.PrimVector arr s l) = fst . unsafeDupablePerformIO $ do
    allocPrimVectorUnsafe (base64EncodeLength l) $ \ buf# ->
        withPrimArrayUnsafe arr $ \ parr _ ->
            hs_base64_encode buf# 0 parr s l

-- | Return the encoded length of a given input length, always a multipler of 4.
base64EncodeLength :: Int -> Int
base64EncodeLength n = ((n+2) `quot` 3) `unsafeShiftL` 2

-- | 'B.Builder' version of 'base64Encode'.
base64EncodeBuilder :: V.Bytes -> B.Builder ()
base64EncodeBuilder (V.PrimVector arr s l) =
    B.writeN (base64EncodeLength l) (\ (MutablePrimArray mba#) i -> do
        withPrimArrayUnsafe arr $ \ parr _ ->
            hs_base64_encode mba# i parr s l)

-- | Text version of 'base64Encode'.
base64EncodeText :: V.Bytes -> T.Text
base64EncodeText = T.Text . base64Encode

-- | Decode a base64 encoding string, return Nothing on illegal bytes or incomplete input.
base64Decode :: V.Bytes -> Maybe V.Bytes
base64Decode ba
    | inputLen == 0 = Just V.empty
    | decodeLen == -1 = Nothing
    | otherwise = unsafeDupablePerformIO $ do
        ((V.PrimVector arr s' _), r) <- withPrimVectorUnsafe ba $ \ ba# s l ->
            allocPrimVectorUnsafe decodeLen $ \ buf# ->
                hs_base64_decode buf# ba# s l
        if r == 0
        then return Nothing
        else return (Just (V.PrimVector arr s' r))
  where
    inputLen = V.length ba
    decodeLen = base64DecodeLength inputLen

-- | Exception during base64 decoding.
data Base64DecodeException = IllegalBase64Bytes V.Bytes CallStack
                           | IncompleteBase64Bytes V.Bytes CallStack
                        deriving Show
instance Exception Base64DecodeException

-- | Decode a base64 encoding string, throw 'Base64DecodeException' on error.
base64Decode' :: HasCallStack => V.Bytes -> V.Bytes
base64Decode' ba
    | inputLen == 0 = V.empty
    | decodeLen == -1 = throw (IncompleteBase64Bytes ba callStack)
    | otherwise = unsafeDupablePerformIO $ do
        ((V.PrimVector arr s' _), r) <- withPrimVectorUnsafe ba $ \ ba# s l ->
            allocPrimVectorUnsafe decodeLen $ \ buf# ->
                hs_base64_decode buf# ba# s l
        if r == 0
        then throwIO (IllegalBase64Bytes ba callStack)
        else return (V.PrimVector arr s' r)
  where
    inputLen = V.length ba
    decodeLen = base64DecodeLength inputLen

-- | Return the upper bound of decoded length of a given input length
-- , return -1 if illegal(not a multipler of 4).
base64DecodeLength :: Int -> Int
base64DecodeLength n | n .&. 3 == 1 = -1
                     | otherwise = (n `unsafeShiftR` 2) * 3

--------------------------------------------------------------------------------

foreign import ccall unsafe hs_base64_encode :: MBA# Word8 -> Int -> BA# Word8 -> Int -> Int -> IO ()
foreign import ccall unsafe hs_base64_decode :: MBA# Word8 -> BA# Word8 -> Int -> Int -> IO Int
