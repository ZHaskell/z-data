{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Z.Data.Text
Description : Unicode text processing
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

A 'Text' wrap a 'Bytes' which will be interpreted using UTF-8 encoding. User should always use 'validate' \/ 'validateMaybe' to construt a 'Text' (instead of using construtor directly or coercing), otherwise illegal UTF-8 encoded codepoints will cause undefined behaviours.

This library also provide simple unicode processing based on <https://github.com/haskell-Z/utf8rewind/ utf8rewind>,
see 'normalize', 'caseFold' (current using unicode 13 databases).

-}

module Z.Data.Text (
  -- * Text type
    Text(..)
  , validate
  , InvalidUTF8Exception(..)
  , validateMaybe
  , index, indexMaybe, indexR, indexMaybeR
  -- * Basic creating
  , empty, singleton, copy
  , replicate, cycleN
  -- * Conversion between list
  , pack, packN, packR, packRN
  , unpack, unpackR
  -- * Conversion between codepoint vector
  , fromVector
  , toVector
  -- * Basic interface
  , null
  , length
  , append
  , map', imap'
  , foldl', ifoldl'
  , foldr', ifoldr'
  , concat, concatMap
    -- ** Special folds
  , count, all, any
  -- * Slice manipulation
  , cons, snoc
  , uncons, unsnoc
  , headMaybe, tailMayEmpty
  , lastMaybe, initMayEmpty
  , inits, tails
  , take, drop, takeR, dropR
  , slice
  , splitAt
  , takeWhile, takeWhileR, dropWhile, dropWhileR, dropAround
  , break, span
  , breakR, spanR, breakOn, breakOnAll
  , group, groupBy
  , stripPrefix, stripSuffix
  , split, splitWith, splitOn
  , isPrefixOf, isSuffixOf, isInfixOf
  , commonPrefix
  , words, lines, unwords, unlines
  , padLeft, padRight
  -- * Transform
  , reverse
  , intersperse
  , intercalate
  , intercalateElem
  , transpose
  -- * Search
  -- ** searching by equality
  , elem, notElem
  -- ** element-wise search
  , find, findR
  , filter, partition
  -- * Unicode processing
    -- ** normalization
  , NormalizationResult(..), NormalizeMode(..)
  , isNormalized, isNormalizedTo, normalize, normalizeTo
    -- ** Case conversion
    -- $case
  , Locale(..)
  , envLocale
  , caseFold, caseFoldWith, toLower, toLowerWith, toUpper, toUpperWith, toTitle, toTitleWith
    -- ** Unicode category
  , isCategory, spanCategory
  , Category(..)
 ) where

import           Z.Data.Text.Base
import           Z.Data.Text.Search
import           Z.Data.Text.Extra
import           Prelude                  ()


