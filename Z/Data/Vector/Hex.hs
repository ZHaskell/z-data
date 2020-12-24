{-|
Module      : Z.Data.Vector.Hex
Description : Hex codec for bytes.
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides hex encoding & decoding tools, as well as 'HexBytes' newtype with hex textual instances.

-}

module Z.Data.Vector.Hex
  ( -- * The HexBytes type
    HexBytes(..)
  -- * Encoding & Decoding functions
  , hexEncode
  , hexEncodeText
  , hexEncodeBuilder
  , hexDecode
  , hexDecode'
  , HexDecodeException(..)
  -- * Internal C FFIs
  ,  hs_hex_encode, hs_hex_encode_upper, hs_hex_decode
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
import qualified Z.Data.Text.Print          as T
import qualified Z.Data.JSON                as JSON
import           Z.Foreign

-- | New type wrapper for 'V.Bytes' with hex encoding(uppercase) Show\/JSON instances.
newtype HexBytes = HexBytes { unHexBytes :: V.Bytes }
    deriving (Eq, Ord)
    deriving newtype (Monoid, Semigroup, Hashable)

instance Show HexBytes where
    show (HexBytes bs) = T.unpack $ hexEncodeText True bs

instance T.Print HexBytes where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (HexBytes bs) = B.quotes (hexEncodeBuilder True bs)

instance JSON.FromValue HexBytes where
    {-# INLINE fromValue #-}
    fromValue = JSON.withText "Z.Data.Text.HexBytes" $ \ t ->
        case hexDecode (T.getUTF8Bytes t) of
            Just bs -> return (HexBytes bs)
            Nothing -> JSON.fail' "illegal hex encoding bytes"

instance JSON.ToValue HexBytes where
    {-# INLINE toValue #-}
    toValue (HexBytes bs) = JSON.String (hexEncodeText True bs)

instance JSON.EncodeJSON HexBytes where
    {-# INLINE encodeJSON #-}
    encodeJSON (HexBytes bs) = hexEncodeBuilder True bs

-- | Encode 'V.Bytes' using hex(base16) encoding.
hexEncode :: Bool   -- ^ uppercase?
          -> V.Bytes -> V.Bytes
{-# INLINE hexEncode #-}
hexEncode upper (V.PrimVector arr s l) = fst . unsafeDupablePerformIO $ do
    allocPrimVectorUnsafe (l `unsafeShiftL` 1) $ \ buf# ->
        withPrimArrayUnsafe arr $ \ parr _ ->
            if upper
            then hs_hex_encode_upper buf# 0 parr s l
            else hs_hex_encode buf# 0 parr s l


-- | 'B.Builder' version of 'hexEncode'.
hexEncodeBuilder :: Bool -- ^ uppercase?
                 -> V.Bytes -> B.Builder ()
{-# INLINE hexEncodeBuilder #-}
hexEncodeBuilder upper (V.PrimVector arr s l) =
    B.writeN (l `unsafeShiftL` 1) (\ (MutablePrimArray mba#) i -> do
        withPrimArrayUnsafe arr $ \ parr _ ->
            if upper
            then hs_hex_encode_upper mba# i parr s l
            else hs_hex_encode mba# i parr s l)

-- | Text version of 'hexEncode'.
hexEncodeText :: Bool   -- ^ uppercase?
              -> V.Bytes -> T.Text
{-# INLINE hexEncodeText #-}
hexEncodeText upper = T.Text . hexEncode upper

-- | Decode a hex encoding string, return Nothing on illegal bytes or incomplete input.
hexDecode :: V.Bytes -> Maybe V.Bytes
{-# INLINABLE hexDecode #-}
hexDecode ba
    | V.length ba == 0 = Just V.empty
    | V.length ba .&. 1 == 1 = Nothing
    | otherwise = unsafeDupablePerformIO $ do
        (out, r) <- withPrimVectorUnsafe ba $ \ ba# s l ->
            allocPrimVectorUnsafe (l `unsafeShiftR` 1) $ \ buf# ->
                hs_hex_decode buf# ba# s l
        if r < 0
        then return Nothing
        else return (Just out)

-- | Exception during hex decoding.
data HexDecodeException = IllegalHexBytes V.Bytes CallStack
                        | IncompleteHexBytes V.Bytes CallStack
                    deriving Show
instance Exception HexDecodeException

-- | Decode a hex encoding string, throw 'HexDecodeException' on error.
hexDecode' :: HasCallStack => V.Bytes -> V.Bytes
{-# INLINABLE hexDecode' #-}
hexDecode' ba
    | V.length ba == 0 = V.empty
    | V.length ba .&. 1 == 1 = throw (IncompleteHexBytes ba callStack)
    | otherwise = unsafeDupablePerformIO $ do
        (out, r) <- withPrimVectorUnsafe ba $ \ ba# s l ->
            allocPrimVectorUnsafe (l `unsafeShiftR` 1) $ \ buf# ->
                hs_hex_decode buf# ba# s l
        if r < 0
        then throwIO (IllegalHexBytes ba callStack)
        else return out

--------------------------------------------------------------------------------

foreign import ccall unsafe hs_hex_encode :: MBA# Word8 -> Int -> BA# Word8 -> Int -> Int -> IO ()
foreign import ccall unsafe hs_hex_encode_upper :: MBA# Word8 -> Int -> BA# Word8 -> Int -> Int -> IO ()
foreign import ccall unsafe hs_hex_decode :: MBA# Word8 -> BA# Word8 -> Int -> Int -> IO Int
