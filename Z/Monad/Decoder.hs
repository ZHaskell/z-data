{-|
Module      : Z.Data.Parser
Description : Efficient deserialization/parse.
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide a simple resumable 'Parser', which is suitable for binary protocol and simple textual protocol parsing. Both binary parsers ('decodePrim' ,etc) and textual parsers are provided, and they all work on 'V.Bytes'.

You can use 'Alternative' instance to do backtracking, each branch will either succeed and may consume some input, or fail without consume anything. It's recommend to use 'peek' or 'peekMaybe' to avoid backtracking if possible to get high performance.

Error message can be attached using '<?>', which have very small overhead, so it's recommended to attach a message in front of a composed parser like @xPacket = "Foo.Bar.xPacket" <?> do ...@, following is an example message when parsing an integer failed:

@
    >parse int "foo"
    ([102,111,111],Left ["Z.Data.Parser.Numeric.int","Std.Data.Parser.Base.takeWhile1: no satisfied byte"])
    -- It's easy to see we're trying to match a leading sign or digit here
@

Use <https://hackage.haskell.org/package/parser-combinators parser-combinators> to get combinators based on
'Applicative' or 'Monad' instance, such as @manyTill@, @sepBy@, etc.

-}
module Z.Data.Decoder
  ( -- * Parser types
    Result(..)
  , ParseError
  , Parser
  , (<?>)
    -- * Running a parser
  , parse, parse', parseChunk, parseChunkList, ParseChunks, parseChunks, finishParsing
  , runAndKeepTrack, match
    -- * Basic parsers
  , ensureN, endOfInput, atEnd
    -- * Primitive decoders
  , decodePrim, BE(..), LE(..)
  , decodePrimLE, decodePrimBE
    -- * More parsers
  , scan, scanChunks, peekMaybe, peek, satisfy, satisfyWith
  , anyWord8, word8, char8, anyChar8, anyCharUTF8, charUTF8, char7, anyChar7
  , skipWord8, endOfLine, skip, skipWhile, skipSpaces
  , take, takeN, takeTill, takeWhile, takeWhile1, takeRemaining, takeUTF8, bytes, bytesCI
  , text
    -- * Numeric parsers
    -- ** Decimal
  , uint, int, integer
  , uint_, int_, digit
    -- ** Hex
  , hex, hex', hex_
    -- ** Fractional
  , rational
  , float, double
  , scientific
  , scientifically
    -- * Stricter fractional(rfc8259)
  , rational'
  , float', double'
  , scientific'
  , scientifically'
  -- * Time
  , day
  , localTime
  , timeOfDay
  , timeZone
  , utcTime
  , zonedTime
  -- * UUID
  , uuid, decodeUUID
    -- * Misc
  , fail', failWithInput, unsafeLiftIO
    -- * Specialized primitive parser
  , decodeWord  , decodeWord64, decodeWord32, decodeWord16, decodeWord8
  , decodeInt   , decodeInt64 , decodeInt32 , decodeInt16 , decodeInt8 , decodeDouble, decodeFloat
  , decodeWordLE  , decodeWord64LE , decodeWord32LE , decodeWord16LE
  , decodeIntLE   , decodeInt64LE , decodeInt32LE , decodeInt16LE , decodeDoubleLE , decodeFloatLE
  , decodeWordBE  , decodeWord64BE , decodeWord32BE , decodeWord16BE
  , decodeIntBE   , decodeInt64BE , decodeInt32BE , decodeInt16BE , decodeDoubleBE , decodeFloatBE
  ) where

import           Z.Data.Parser.Base
import           Z.Data.Parser.Numeric
import           Z.Data.Parser.Time
import           Z.Data.Parser.UUID
import           Prelude hiding (take, takeWhile, decodeFloat)
