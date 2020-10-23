{-|
Module      : Z.Data.Vector.QQ
Description : vectors literals using QuasiQuote
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides functions for writing vector literals using 'QuasiQuote' similar to "Z.Data.Array.QQ" module.

@
> :set -XQuasiQuotes
> :t [vecASCII|asdfg|]
[vecASCII|asdfg|] :: Z.Data.Vector.Base.PrimVector GHC.Word.Word8
> [vecASCII|asdfg|]
[97,115,100,102,103]
> :t [vecI16|1,2,3,4,5|]
[vecI16|1,2,3,4,5|] :: Z.Data.Vector.Base.PrimVector GHC.Int.Int16
> [vecI16|1,2,3,4,5|]
[1,2,3,4,5]
@

-}

module Z.Data.Vector.QQ (
  -- * QuasiQuoters
    vecASCII
  , vecW8, vecW16, vecW32, vecW64, vecWord
  , vecI8, vecI16, vecI32, vecI64, vecInt
  ) where

import qualified Language.Haskell.TH.Quote      as QQ
import           Z.Data.Array.QQ                as QQ
import           Z.Data.Vector.Base

--------------------------------------------------------------------------------
-- Quoters

vecASCII :: QQ.QuasiQuoter
vecASCII = QQ.QuasiQuoter
    (asciiLiteral $ \ len addr -> [| PrimVector (QQ.word8ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecASCII as a pattern")
    (error "Cannot use vecASCII as a type")
    (error "Cannot use vecASCII as a dec")

vecW8 :: QQ.QuasiQuoter
vecW8 = QQ.QuasiQuoter
    (QQ.word8Literal $ \ len addr -> [| PrimVector (QQ.word8ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecW8 as a pattern")
    (error "Cannot use vecW8 as a type")
    (error "Cannot use vecW8 as a dec")

vecW16 :: QQ.QuasiQuoter
vecW16 = QQ.QuasiQuoter
    (QQ.word16Literal $ \ len addr -> [| PrimVector (QQ.word16ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecW16 as a pattern")
    (error "Cannot use vecW16 as a type")
    (error "Cannot use vecW16 as a dec")

vecW32 :: QQ.QuasiQuoter
vecW32 = QQ.QuasiQuoter
    (QQ.word32Literal $ \ len addr -> [| PrimVector (QQ.word32ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecW32 as a pattern")
    (error "Cannot use vecW32 as a type")
    (error "Cannot use vecW32 as a dec")

vecW64 :: QQ.QuasiQuoter
vecW64 = QQ.QuasiQuoter
    (QQ.word64Literal $ \ len addr -> [| PrimVector (QQ.word64ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecW64 as a pattern")
    (error "Cannot use vecW64 as a type")
    (error "Cannot use vecW64 as a dec")

vecWord :: QQ.QuasiQuoter
vecWord = QQ.QuasiQuoter
    (QQ.wordLiteral $ \ len addr ->
        [| PrimVector (QQ.wordArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecWord as a pattern")
    (error "Cannot use vecWord as a type")
    (error "Cannot use vecWord as a dec")

vecI8 :: QQ.QuasiQuoter
vecI8 = QQ.QuasiQuoter
    (QQ.int8Literal $ \ len addr ->
        [| PrimVector (QQ.int8ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecI8 as a pattern")
    (error "Cannot use vecI8 as a type")
    (error "Cannot use vecI8 as a dec")

vecI16 :: QQ.QuasiQuoter
vecI16 = QQ.QuasiQuoter
    (QQ.int16Literal $ \ len addr ->
        [| PrimVector (QQ.int16ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecI16 as a pattern")
    (error "Cannot use vecI16 as a type")
    (error "Cannot use vecI16 as a dec")

vecI32 :: QQ.QuasiQuoter
vecI32 = QQ.QuasiQuoter
    (QQ.int32Literal $ \ len addr ->
        [| PrimVector (QQ.int32ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecI32 as a pattern")
    (error "Cannot use vecI32 as a type")
    (error "Cannot use vecI32 as a dec")

vecI64 :: QQ.QuasiQuoter
vecI64 = QQ.QuasiQuoter
    (QQ.int64Literal $ \ len addr ->
        [| PrimVector (QQ.int64ArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecI64 as a pattern")
    (error "Cannot use vecI64 as a type")
    (error "Cannot use vecI64 as a dec")

vecInt :: QQ.QuasiQuoter
vecInt = QQ.QuasiQuoter
    (QQ.intLiteral $ \ len addr ->
        [| PrimVector (QQ.intArrayFromAddr $(len) $(addr)) 0 $(len) |])
    (error "Cannot use vecInt as a pattern")
    (error "Cannot use vecInt as a type")
    (error "Cannot use vecInt as a dec")
