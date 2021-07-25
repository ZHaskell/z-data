{-|
Module      : Z.Data.Builder
Description : Efficient serialization/format.
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

A 'Builder' records a buffer writing function, which can be 'mappend' in O(1)
via composition. This module provides many functions to turn basic data types
into 'Builder's, which can used to build strict 'Bytes' or list of 'Bytes' chunks.

-}

module Z.Data.Builder
  ( -- * Builder type
    Builder
  , append
   -- * Running builders
  , build
  , buildWith
  , buildChunks
  , buildChunksWith
  , buildText
  , unsafeBuildText
    -- * Basic buiders
  , bytes
  , ensureN
  , writeN
   -- * Pritimive builders
  , encodePrim
  , BE(..), LE(..)
  , encodePrimLE
  , encodePrimBE
  -- * More builders
  , stringModifiedUTF8, charModifiedUTF8, stringUTF8
  , charUTF8, string7, char7, word7, string8, char8, word8, word8N, text
  -- * Numeric builders
  -- ** Integral type formatting
  , IFormat(..)
  , defaultIFormat
  , Padding(..)
  , int
  , intWith
  , integer
  -- ** Fixded size hexidecimal formatting
  , hex, hexUpper
  -- ** IEEE float formating
  , FFormat(..)
  , double
  , doubleWith
  , float
  , floatWith
  , scientific
  , scientific'
  , scientificWith
    -- * Builder helpers
  , paren, parenWhen, curly, square, angle, quotes, squotes, colon, comma, intercalateVec, intercalateList
    -- * Time
  , day
  , timeOfDay
  , timeZone
  , utcTime
  , localTime
  , zonedTime
    -- * UUID
  , uuid, uuidUpper, encodeUUID
    -- * Specialized primitive builder
  , encodeWord  , encodeWord64, encodeWord32, encodeWord16, encodeWord8
  , encodeInt   , encodeInt64 , encodeInt32 , encodeInt16 , encodeInt8 , encodeDouble, encodeFloat
  , encodeWordLE  , encodeWord64LE , encodeWord32LE , encodeWord16LE
  , encodeIntLE   , encodeInt64LE , encodeInt32LE , encodeInt16LE , encodeDoubleLE , encodeFloatLE
  , encodeWordBE  , encodeWord64BE , encodeWord32BE , encodeWord16BE
  , encodeIntBE   , encodeInt64BE , encodeInt32BE , encodeInt16BE , encodeDoubleBE , encodeFloatBE
  ) where

import           Z.Data.Builder.Base
import           Z.Data.Builder.Numeric
import           Z.Data.Builder.Time
import           Z.Data.Builder.UUID
import           Prelude                        ()
