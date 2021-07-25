{-|
Module:      Z.Data.Parser.UUID
Description : Parsers for UUID.
Copyright:   (c) 2020 Dong Han
License:     BSD3
Maintainer:  Dong <winterland1989@gmail.com>
Stability:   experimental
Portability: portable

Parsers for parsing UUID.
-}

module Z.Data.Parser.UUID
    ( uuid
    , decodeUUID
    ) where

import           Z.Data.ASCII
import qualified Z.Data.Parser.Base         as P
import qualified Z.Data.Parser.Numeric      as P
import           Data.UUID.Types.Internal

-- | Parse texutal UUID bytes(lower or upper-cased), e.g. @550e8400-e29b-41d4-a716-446655440000@
uuid :: P.Parser UUID
{-# INLINABLE uuid #-}
uuid =  do
    p1 <- P.takeN isHexDigit 8
    P.word8 HYPHEN
    p2 <- P.takeN isHexDigit 4
    P.word8 HYPHEN
    p3 <- P.takeN isHexDigit 4
    P.word8 HYPHEN
    p4 <- P.takeN isHexDigit 4
    P.word8 HYPHEN
    p5 <- P.takeN isHexDigit 12

    let !w1 = P.hexLoop (P.hexLoop (P.hexLoop 0 p1) p2) p3
        !w2 = P.hexLoop (P.hexLoop 0 p4) p5

    pure (UUID w1 w2)

-- | Decode binary UUID(two 64-bits word in big-endian), as described in <http://tools.ietf.org/html/rfc4122 RFC 4122>. 
decodeUUID :: P.Parser UUID
{-# INLINABLE  decodeUUID #-}
decodeUUID = UUID <$> P.decodeWord64BE <*> P.decodeWord64BE