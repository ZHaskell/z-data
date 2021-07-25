{-|
Module:      Z.Data.Builder.UUID
Description : Builders for UUID.
Copyright:   (c) 2021 Dong Han
License:     BSD3
Maintainer:  Dong <winterland1989@gmail.com>
Stability:   experimental
Portability: portable

Builders for UUID.
-}

module Z.Data.Builder.UUID
    ( uuid, uuidUpper
    , encodeUUID
    ) where

import           Data.UUID.Types.Internal
import           Data.Word
import           Data.Bits
import           Z.Data.ASCII
import qualified Z.Data.Builder.Base         as B
import qualified Z.Data.Builder.Numeric      as B

-- | Write texutal UUID bytes, e.g. @550e8400-e29b-41d4-a716-446655440000@
uuid :: UUID -> B.Builder ()
{-# INLINABLE uuid #-}
uuid (UUID wh wl) =  do
    let !w1 = fromIntegral @Word64 @Word32 $ wh `unsafeShiftR` 32
        !w2 = fromIntegral @Word64 @Word16 $ wh `unsafeShiftR` 16 .&. 0xFFFF
        !w3 = fromIntegral @Word64 @Word16 $ wh .&. 0xFFFF
        !w4 = fromIntegral @Word64 @Word16 $ wl `unsafeShiftR` 48
        !w5 = fromIntegral @Word64 @Word16 $ wl `unsafeShiftR` 32 .&. 0xFFFF
        !w6 = fromIntegral @Word64 @Word32 $ wl .&. 0xFFFFFFFF
    B.hex w1
    B.word8 HYPHEN
    B.hex w2
    B.word8 HYPHEN
    B.hex w3
    B.word8 HYPHEN
    B.hex w4
    B.word8 HYPHEN
    B.hex w5
    B.hex w6

-- | Write texutal UUID bytes, e.g. @550e8400-e29b-41d4-a716-446655440000@
uuidUpper :: UUID -> B.Builder ()
{-# INLINABLE uuidUpper #-}
uuidUpper (UUID wh wl) =  do
    let !w1 = fromIntegral @Word64 @Word32 $ wh `unsafeShiftR` 32
        !w2 = fromIntegral @Word64 @Word16 $ wh `unsafeShiftR` 16 .&. 0xFFFF
        !w3 = fromIntegral @Word64 @Word16 $ wh .&. 0xFFFF
        !w4 = fromIntegral @Word64 @Word16 $ wl `unsafeShiftR` 48
        !w5 = fromIntegral @Word64 @Word16 $ wl `unsafeShiftR` 32 .&. 0xFFFF
        !w6 = fromIntegral @Word64 @Word32 $ wl .&. 0xFFFFFFFF
    B.hexUpper w1
    B.word8 HYPHEN
    B.hexUpper w2
    B.word8 HYPHEN
    B.hexUpper w3
    B.word8 HYPHEN
    B.hexUpper w4
    B.word8 HYPHEN
    B.hexUpper w5
    B.hexUpper w6


-- | Encode binary UUID(two 64-bits word in big-endian), as described in <http://tools.ietf.org/html/rfc4122 RFC 4122>. 
encodeUUID :: UUID -> B.Builder ()
{-# INLINABLE  encodeUUID #-}
encodeUUID (UUID wh wl) = do 
    B.encodeWord64BE wh
    B.encodeWord64BE wl