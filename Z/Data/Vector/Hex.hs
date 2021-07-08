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
  , hexDecodeWS
  , hexDecodeWS'
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

instance JSON.JSON HexBytes where
    {-# INLINE fromValue #-}
    fromValue = JSON.withText "Z.Data.Text.HexBytes" $ \ t ->
        case hexDecode (T.getUTF8Bytes t) of
            Just bs -> return (HexBytes bs)
            Nothing -> JSON.fail' "illegal hex encoding bytes"
    {-# INLINE toValue #-}
    toValue (HexBytes bs) = JSON.String (hexEncodeText True bs)
    {-# INLINE encodeJSON #-}
    encodeJSON (HexBytes bs) = hexEncodeBuilder True bs

-- | Encode 'V.Bytes' using hex(base16) encoding.
hexEncode :: Bool   -- ^ uppercase?
          -> V.Bytes -> V.Bytes
{-# INLINABLE hexEncode #-}
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
{-# INLINABLE hexEncodeText #-}
hexEncodeText upper = T.Text . hexEncode upper

-- | Decode a hex encoding string, return Nothing on illegal bytes or incomplete input.
hexDecode :: V.Bytes -> Maybe V.Bytes
{-# INLINABLE hexDecode #-}
hexDecode ba
    | V.length ba == 0 = Just V.empty
    | V.length ba .&. 1 == 1 = Nothing
    | otherwise = unsafeDupablePerformIO $ do
        (arr, r) <- withPrimVectorUnsafe ba $ \ ba# s l ->
            allocPrimArrayUnsafe (l `unsafeShiftR` 1) $ \ buf# ->
                hs_hex_decode buf# ba# s l
        if r < 0
        then return Nothing
        else return (Just (V.PrimVector arr 0 r))

-- | Decode a hex encoding string, ignore ASCII whitespace(space, tab, newline, vertical tab, form feed, carriage return).
--
-- This is useful when you get some hex nibbles by pasting from web, note only whitesapces between bytes(two nibbles) are allowed:
--
-- >>> hexDecodeWS "6f7481 da0e53"
-- Just [111,116,129,218,14,83]
-- >>> hexDecodeWS "6f7481d a0e53"
-- Nothing
--
hexDecodeWS :: V.Bytes -> Maybe V.Bytes
{-# INLINABLE hexDecodeWS #-}
hexDecodeWS ba
    | V.length ba == 0 = Just V.empty
    | otherwise = unsafeDupablePerformIO $ do
        (arr, r) <- withPrimVectorUnsafe ba $ \ ba# s l ->
            allocPrimArrayUnsafe (l `unsafeShiftR` 1) $ \ buf# ->
                hs_hex_decode_ws buf# ba# s l
        if r < 0
        then return Nothing
        else return (Just (V.PrimVector arr 0 r))

-- | Exception during hex decoding.
data HexDecodeException = IllegalHexBytes V.Bytes CallStack
                        | IncompleteHexBytes V.Bytes CallStack
                    deriving Show
instance Exception HexDecodeException

-- | Decode a hex encoding string, throw 'HexDecodeException' on error.
hexDecode' :: HasCallStack => V.Bytes -> V.Bytes
{-# INLINE hexDecode' #-}
hexDecode' ba = case hexDecode ba of
    Just r -> r
    _ -> throw (IllegalHexBytes ba callStack)

-- | Decode a hex encoding string, ignore ASCII whitespace(space, tab, newline, vertical tab, form feed, carriage return), throw 'HexDecodeException' on error.
hexDecodeWS' :: HasCallStack => V.Bytes -> V.Bytes
{-# INLINE hexDecodeWS' #-}
hexDecodeWS' ba = case hexDecodeWS ba of
    Just r -> r
    _ -> throw (IllegalHexBytes ba callStack)

--------------------------------------------------------------------------------

foreign import ccall unsafe hs_hex_encode :: MBA# Word8 -> Int -> BA# Word8 -> Int -> Int -> IO ()
foreign import ccall unsafe hs_hex_encode_upper :: MBA# Word8 -> Int -> BA# Word8 -> Int -> Int -> IO ()
foreign import ccall unsafe hs_hex_decode :: MBA# Word8 -> BA# Word8 -> Int -> Int -> IO Int
foreign import ccall unsafe hs_hex_decode_ws :: MBA# Word8 -> BA# Word8 -> Int -> Int -> IO Int
