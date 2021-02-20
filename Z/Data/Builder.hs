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
  , scientificWith
    -- * Builder helpers
  , paren, curly, square, angle, quotes, squotes, colon, comma, intercalateVec, intercalateList
    -- * Time
  , day
  , timeOfDay
  , timeZone
  , utcTime
  , localTime
  , zonedTime
  ) where

import           Z.Data.Builder.Base
import           Z.Data.Builder.Numeric
import           Z.Data.Builder.Time
