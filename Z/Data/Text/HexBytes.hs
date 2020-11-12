{-|
Module      : Z.Data.CBytes
Description : Hex codec for bytes.
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides hex encoding & decoding tools, as well as 'HexBytes' newtype with hex textual instances.

-}

module Z.Data.Text.HexBytes
  ( -- * The HexBytes type
    HexBytes(..)
  -- * Encoding & Decoding functions
    , hexEncode, hexEncodeText, hexEncodeUpper, hexEncodeUpperText
    , hexDecode, hexDecodeText
    , hexDecode', hexDecodeText'
  ) where

import           Control.Exception
import           Data.Word
import           Data.Bits                      (unsafeShiftR, (.&.))
import           Data.Hashable                  (Hashable(..))
import           GHC.Stack
import           System.IO.Unsafe
import qualified Z.Data.Vector as V
import qualified Z.Data.Text.Base as T
import qualified Z.Data.Text.ShowT as T
import qualified Z.Data.Builder.Base as B
import           Z.Foreign

-- | New type wrapper for 'V.Bytes' with hex encoding(uppercase) Show\/JSON instances.
newtype HexBytes = HexBytes { getHexBytes :: V.Bytes }
    deriving (Eq, Ord)
    deriving newtype (Monoid, Semigroup, Hashable)

instance Show HexBytes where
    show (HexBytes bs) = T.unpack $ hexEncodeUpperText bs

instance T.ShowT HexBytes where
    toTextBuilder _ (HexBytes bs) = T.unsafeFromBuilder $ B.quotes (B.hexBytes bs)

hexEncode :: V.Bytes -> V.Bytes
hexEncode = B.hexEncodeBytes False

hexEncodeUpper :: V.Bytes -> V.Bytes
hexEncodeUpper = B.hexEncodeBytes True

hexEncodeText :: V.Bytes -> T.Text
hexEncodeText = T.Text . B.hexEncodeBytes False

hexEncodeUpperText :: V.Bytes -> T.Text
hexEncodeUpperText = T.Text . B.hexEncodeBytes True

-- | Decode a hex encoding string, return Nothing on illegal bytes or incomplete input.
hexDecode :: V.Bytes -> Maybe V.Bytes
hexDecode ba
    | V.length ba .&. 1 == 1 = Nothing
    | otherwise = unsafeDupablePerformIO $ do
        (out, r) <- withPrimVectorUnsafe ba $ \ ba# s l ->
            allocPrimVectorUnsafe (l `unsafeShiftR` 1) $ \ buf# ->
                hs_hex_decode buf# ba# s l
        if r < 0
        then return Nothing
        else return (Just out)

data DecodeException = IllegalByte V.Bytes Int CallStack
                     | IncompleteInput V.Bytes CallStack
                    deriving Show
instance Exception DecodeException

-- | Decode a hex encoding string, throw 'DecodeException' on error.
hexDecode' :: HasCallStack => V.Bytes -> V.Bytes
hexDecode' ba
    | V.length ba .&. 1 == 1 = throw (IncompleteInput ba callStack)
    | otherwise = unsafeDupablePerformIO $ do
        (out, r) <- withPrimVectorUnsafe ba $ \ ba# s l ->
            allocPrimVectorUnsafe (l `unsafeShiftR` 1) $ \ buf# ->
                hs_hex_decode buf# ba# s l
        if r < 0
        then throwIO (IllegalByte ba (0-r-1) callStack)
        else return out

hexDecodeText :: T.Text -> Maybe V.Bytes
hexDecodeText = hexDecode . T.getUTF8Bytes

hexDecodeText' :: HasCallStack => T.Text -> V.Bytes
hexDecodeText' = hexDecode' . T.getUTF8Bytes

foreign import ccall unsafe hs_hex_decode :: MBA# Word8 -> BA# Word8 -> Int -> Int -> IO Int
