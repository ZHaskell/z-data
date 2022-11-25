{-|
Module      : Z.Data.Array.QQ
Description : Extra stuff for PrimArray related literals
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides functions for writing 'PrimArray' related literals 'QuasiQuote'.

@
> :set -XQuasiQuotes
> :t [arrASCII|asdfg|]
[arrASCII|asdfg|] :: PrimArray GHC.Word.Word8
> [arrASCII|asdfg|]
fromListN 5 [97,115,100,102,103]
> :t [arrI16|1,2,3,4,5|]
[arrI16|1,2,3,4,5|] :: PrimArray GHC.Int.Int16
> [arrI16|1,2,3,4,5|]
fromListN 5 [1,2,3,4,5]
@

-}

module Z.Data.Array.QQ
  ( -- * PrimArray literal quoters
    arrASCII
  , arrW8, arrW16, arrW32, arrW64, arrWord
  , arrI8, arrI16, arrI32, arrI64, arrInt
   -- * quoter helpers
  , asciiLiteral
  , utf8Literal
  , arrayLiteral
  , word8Literal
  , word16Literal
  , word32Literal
  , word64Literal
  , wordLiteral
  , int8Literal
  , int16Literal
  , int32Literal
  , int64Literal
  , intLiteral
  , word8ArrayFromAddr
  , word16ArrayFromAddr
  , word32ArrayFromAddr
  , word64ArrayFromAddr
  , wordArrayFromAddr
  , int8ArrayFromAddr
  , int16ArrayFromAddr
  , int32ArrayFromAddr
  , int64ArrayFromAddr
  , intArrayFromAddr
  ) where

#include "MachDeps.h"

import           Control.Monad
import           Data.Bits
import           Data.Char                 (ord)
import           Data.Primitive.PrimArray hiding (copyPtrToMutablePrimArray)
import           GHC.Exts
import           Data.Word
import           Data.Int
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Z.Data.Array
import           Control.Monad.ST

-- $asciiLiteralExample
-- @
-- arrASCII :: QuasiQuoter
-- arrASCII = QuasiQuoter
--     (asciiLiteral $ \ len addr -> [| word8ArrayFromAddr $(len) $(addr) |])
--     ...
--
-- word8ArrayFromAddr :: Int -> Addr# -> PrimArray Word8
-- {-# INLINE word8ArrayFromAddr #-}
-- word8ArrayFromAddr l addr# = runST $ do
--     mba <- newPrimArray (I# l)
--     copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
--     unsafeFreezePrimArray mba
-- @

-- | Construct data with ASCII encoded literals.
--
-- Provide a packing function, return a packing expression. Example usage:
--
-- $asciiLiteralExample
asciiLiteral :: (ExpQ -> ExpQ -> ExpQ) -- ^ Construction function which receive a byte
                                       --   length 'Int' and a 'Addr#' 'LitE' expression.
             -> String                 -- ^ Quoter input
             -> ExpQ                   -- ^ Final Quoter
asciiLiteral k str = k (return . LitE  . IntegerL . fromIntegral $ length str)
                       ((LitE . StringPrimL) `fmap` check str)
  where
    check :: String -> Q [Word8]
    check [] = return []
    check (c:cs) = do
        when (ord c > 0xFF) $
            fail $ "character '" ++ [c] ++ "' is have out of range in ASCII literal:" ++ str
        cs' <- check cs
        return (fromIntegral (ord c):cs')


-- | @[arrASCII|asdfg|] :: PrimArray Word8@
arrASCII :: QuasiQuoter
arrASCII = QuasiQuoter
    (asciiLiteral $ \ len addr -> [| word8ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrASCII as a pattern")
    (error "Cannot use arrASCII as a type")
    (error "Cannot use arrASCII as a dec")

word8ArrayFromAddr :: Int -> Addr# -> PrimArray Word8
{-# INLINABLE word8ArrayFromAddr #-}
word8ArrayFromAddr l addr# = runST $ do
    mba <- newPrimArray l
    copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
    unsafeFreezePrimArray mba

int8ArrayFromAddr :: Int -> Addr# -> PrimArray Int8
{-# INLINE int8ArrayFromAddr #-}
int8ArrayFromAddr l addr# = castArray (word8ArrayFromAddr l addr#)


-- | Construct data with UTF8 encoded literals.
--
-- See 'asciiLiteral'
utf8Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
utf8Literal k str = k (return . LitE  . IntegerL . fromIntegral $ length str)
                      ((LitE . StringPrimL) `fmap` check str)
  where
    check :: String -> Q [Word8]
    check [] = return []
    check (c:cs) = case ord c of
        n
            | n <= 0x0000007F -> do
                let w = fromIntegral n
                ws <- check cs
                return (w:ws)
            | n <= 0x000007FF -> do
                let w1 = fromIntegral $ 0xC0 .|. (n `shiftR` 6)
                    w2 = fromIntegral $ 0x80 .|. (n .&. 0x3F)
                ws <- check cs
                return (w1:w2:ws)
            | n <= 0x0000D7FF -> do
                let w1 = fromIntegral $ 0xE0 .|. (n `shiftR` 12)
                    w2 = fromIntegral $ 0x80 .|. (n `shiftR` 6 .&. 0x3F)
                    w3 = fromIntegral $ 0x80 .|. (n .&. 0x3F)
                ws <- check cs
                return (w1:w2:w3:ws)
            | n <= 0x0000DFFF -> do
                fail $ "character '" ++ [c] ++ "' is have out of range in UTF-8 literal:" ++ str
            | n <= 0x0000FFFF -> do
                let w1 = fromIntegral $ 0xE0 .|. (n `shiftR` 12)
                    w2 = fromIntegral $ 0x80 .|. (n `shiftR` 6 .&. 0x3F)
                    w3 = fromIntegral $ 0x80 .|. (n .&. 0x3F)
                ws <- check cs
                return (w1:w2:w3:ws)
            | n <= 0x0010FFFF -> do
                let w1 = fromIntegral $ 0xF0 .|. (n `shiftR` 18)
                    w2 = fromIntegral $ 0x80 .|. (n `shiftR` 12 .&. 0x3F)
                    w3 = fromIntegral $ 0x80 .|. (n `shiftR` 6 .&. 0x3F)
                    w4 = fromIntegral $ 0x80 .|. (n .&. 0x3F)
                ws <- check cs
                return (w1:w2:w3:w4:ws)
            | otherwise ->
                fail $ "character '" ++ [c] ++ "' is have out of range in UTF-8 literal:" ++ str


-- | Construct data with array literals @e.g. 1,2,3@.
arrayLiteral :: ([Integer] -> Q [Word8])
              -> (ExpQ -> ExpQ -> ExpQ)
              -> String -> ExpQ
arrayLiteral f k str = do
    (len, ws) <- parse str
    k (return . LitE  . IntegerL .fromIntegral $ len) $ (return . LitE . StringPrimL) ws
  where
    parse :: String -> Q (Int, [Word8])
    parse str' = do
        case (readList :: ReadS [Integer]) ("[" ++ str' ++ "]") of
            [(is, "")] -> (length is, ) `fmap` f is
            _ -> do _ <- fail $ "can't parse vector literal:" ++ str'
                    return (0, [])

--------------------------------------------------------------------------------

#define ARRAY_LITERAL_DOC(T)  \
-- | Construct 'PrimArray' 'T' with array literals @e.g. 1,2,3@. See 'asciiLiteral'

ARRAY_LITERAL_DOC(Word8)
word8Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
word8Literal k str = arrayLiteral checkW8 k str
  where
    checkW8 :: [Integer] -> Q [Word8]
    checkW8 [] = return []
    checkW8 (i:is) = do
        when (i<0 || i > 0xFF) $
            fail $ "integer " ++ show i ++ " is out of Word8 range in literal:" ++ str
        ws <- checkW8 is
        let w = fromIntegral (i .&. 0xFF)
        return (w:ws)

-- | @[arrW8|1,2,3,4,5|] :: PrimArray Word8@
arrW8 :: QuasiQuoter
arrW8 = QuasiQuoter
    (word8Literal $ \ len addr -> [| word8ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrW8 as a pattern")
    (error "Cannot use arrW8 as a type")
    (error "Cannot use arrW8 as a dec")

ARRAY_LITERAL_DOC(Int8)
int8Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
int8Literal k str = arrayLiteral checkI8 k str
  where
    checkI8 :: [Integer] -> Q [Word8]
    checkI8 [] = return []
    checkI8 (i:is) = do
        when (i< (-0x80) || i > 0x7F) $
            fail $ "integer " ++ show i ++ " is out of Int8 range in literal:" ++ str
        ws <- checkI8 is
        let w = fromIntegral (i .&. 0xFF)
        return (w:ws)

-- | @[arrW8|1,2,3,4,5|] :: PrimArray Int8@
arrI8 :: QuasiQuoter
arrI8 = QuasiQuoter
    (int8Literal $ \ len addr -> [| int8ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrI8 as a pattern")
    (error "Cannot use arrI8 as a type")
    (error "Cannot use arrI8 as a dec")

--------------------------------------------------------------------------------

ARRAY_LITERAL_DOC(Word16)
word16Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
word16Literal k str = arrayLiteral checkW16 k str
  where
    checkW16 :: [Integer] -> Q [Word8]
    checkW16 [] = return []
    checkW16 (i:is) = do
        when (i<0 || i > 0xFFFF) $
            fail $ "integer " ++ show i ++ " is out of Word16 range in literal:" ++ str
        ws <- checkW16 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w2:w1:ws)
#else
        return (w1:w2:ws)
#endif

-- | @[arrW16|1,2,3,4,5|] :: PrimArray Word16@
arrW16 :: QuasiQuoter
arrW16 = QuasiQuoter
    (word16Literal $ \ len addr -> [| word16ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrW16 as a pattern")
    (error "Cannot use arrW16 as a type")
    (error "Cannot use arrW16 as a dec")

word16ArrayFromAddr :: Int -> Addr# -> PrimArray Word16
{-# INLINE word16ArrayFromAddr #-}
word16ArrayFromAddr l addr# = runST $ do
    mba <- newArr l
    copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
    unsafeFreezePrimArray mba

int16ArrayFromAddr :: Int -> Addr# -> PrimArray Int16
{-# INLINE int16ArrayFromAddr #-}
int16ArrayFromAddr l addr# = castArray (word16ArrayFromAddr l addr#)

ARRAY_LITERAL_DOC(Int16)
int16Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
int16Literal k str = arrayLiteral checkI16 k str
  where
    checkI16 :: [Integer] -> Q [Word8]
    checkI16 [] = return []
    checkI16 (i:is) = do
        when (i<(-0x8000) || i>0x7FFF) $
            fail $ "integer " ++ show i ++ " is out of Int16 range in literal:" ++ str
        ws <- checkI16 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w2:w1:ws)
#else
        return (w1:w2:ws)
#endif

-- | @[arrI16|1,2,3,4,5|] :: PrimArray Int16@
arrI16 :: QuasiQuoter
arrI16 = QuasiQuoter
    (word16Literal $ \ len addr -> [| int16ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrI16 as a pattern")
    (error "Cannot use arrI16 as a type")
    (error "Cannot use arrI16 as a dec")
--------------------------------------------------------------------------------

ARRAY_LITERAL_DOC(Word32)
word32Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
word32Literal k str = arrayLiteral checkW32 k str
  where
    checkW32 :: [Integer] -> Q [Word8]
    checkW32 [] = return []
    checkW32 (i:is) = do
        when (i<0 || i > 0xFFFFFFFF) $
            fail $ "integer " ++ show i ++ " is out of Word32 range in literal:" ++ str
        ws <- checkW32 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w4:w3:w2:w1:ws)
#else
        return (w1:w2:w3:w4:ws)
#endif

-- | @[arrW32|1,2,3,4,5|] :: PrimArray Word32@
arrW32 :: QuasiQuoter
arrW32 = QuasiQuoter
    (word32Literal $ \ len addr -> [| word32ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrW32 as a pattern")
    (error "Cannot use arrW32 as a type")
    (error "Cannot use arrW32 as a dec")

word32ArrayFromAddr :: Int -> Addr# -> PrimArray Word32
{-# INLINE word32ArrayFromAddr #-}
word32ArrayFromAddr l addr# = runST $ do
    mba <- newArr l
    copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
    unsafeFreezePrimArray mba

int32ArrayFromAddr :: Int -> Addr# -> PrimArray Int32
{-# INLINE int32ArrayFromAddr #-}
int32ArrayFromAddr l addr# = castArray (word32ArrayFromAddr l addr#)

ARRAY_LITERAL_DOC(Int32)
int32Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
int32Literal k str = arrayLiteral checkI32 k str
  where
    checkI32 :: [Integer] -> Q [Word8]
    checkI32 [] = return []
    checkI32 (i:is) = do
        when (i<(-0x80000000) || i>0x7FFFFFFF) $
            fail $ "integer " ++ show i ++ " is out of Int32 range in literal:" ++ str
        ws <- checkI32 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w4:w3:w2:w1:ws)
#else
        return (w1:w2:w3:w4:ws)
#endif

-- | @[arrI32|1,2,3,4,5|] :: PrimArray Int32@
arrI32 :: QuasiQuoter
arrI32 = QuasiQuoter
    (int32Literal $ \ len addr -> [| int32ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrI32 as a pattern")
    (error "Cannot use arrI32 as a type")
    (error "Cannot use arrI32 as a dec")

--------------------------------------------------------------------------------

ARRAY_LITERAL_DOC(Word64)
word64Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
word64Literal k str = arrayLiteral checkW64 k str
  where
    checkW64 :: [Integer] -> Q [Word8]
    checkW64 [] = return []
    checkW64 (i:is) = do
        when (i<0 || i > 0xFFFFFFFFFFFFFFFF) $
            fail $ "integer " ++ show i ++ " is out of Word64 range in literal:" ++ str
        ws <- checkW64 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
            w5 = fromIntegral (i `shiftR` 32 .&. 0xFF)
            w6 = fromIntegral (i `shiftR` 40 .&. 0xFF)
            w7 = fromIntegral (i `shiftR` 48 .&. 0xFF)
            w8 = fromIntegral (i `shiftR` 56 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w8:w7:w6:w5:w4:w3:w2:w1:ws)
#else
        return (w1:w2:w3:w4:w5:w6:w7:w8:ws)
#endif

-- | @[arrW64|1,2,3,4,5|] :: PrimArray Word64@
arrW64 :: QuasiQuoter
arrW64 = QuasiQuoter
    (word64Literal $ \ len addr -> [| word64ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrW64 as a pattern")
    (error "Cannot use arrW64 as a type")
    (error "Cannot use arrW64 as a dec")

word64ArrayFromAddr :: Int -> Addr# -> PrimArray Word64
{-# INLINABLE word64ArrayFromAddr #-}
word64ArrayFromAddr l addr# = runST $ do
    mba <- newArr l
    copyPtrToMutablePrimArray mba 0 (Ptr addr#) l
    unsafeFreezePrimArray mba

int64ArrayFromAddr :: Int -> Addr# -> PrimArray Int64
{-# INLINE int64ArrayFromAddr #-}
int64ArrayFromAddr l addr# = castArray (word64ArrayFromAddr l addr#)

ARRAY_LITERAL_DOC(Int64)
int64Literal :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
int64Literal k str = arrayLiteral checkI64 k str
  where
    checkI64 :: [Integer] -> Q [Word8]
    checkI64 [] = return []
    checkI64 (i:is) = do
        when (i<(-0x8000000000000000) || i > 0x7FFFFFFFFFFFFFFF) $
            fail $ "integer " ++ show i ++ " is out of Int64 range in literal:" ++ str
        ws <- checkI64 is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
            w3 = fromIntegral (i `shiftR` 16 .&. 0xFF)
            w4 = fromIntegral (i `shiftR` 24 .&. 0xFF)
            w5 = fromIntegral (i `shiftR` 32 .&. 0xFF)
            w6 = fromIntegral (i `shiftR` 40 .&. 0xFF)
            w7 = fromIntegral (i `shiftR` 48 .&. 0xFF)
            w8 = fromIntegral (i `shiftR` 56 .&. 0xFF)
#ifdef WORDS_BIGENDIAN
        return (w8:w7:w6:w5:w4:w3:w2:w1:ws)
#else
        return (w1:w2:w3:w4:w5:w6:w7:w8:ws)
#endif

-- | @[arrI64|1,2,3,4,5|] :: PrimArray Int64@
arrI64 :: QuasiQuoter
arrI64 = QuasiQuoter
    (int64Literal $ \ len addr -> [| int64ArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrI64 as a pattern")
    (error "Cannot use arrI64 as a type")
    (error "Cannot use arrI64 as a dec")

--------------------------------------------------------------------------------

wordArrayFromAddr :: Int -> Addr# -> PrimArray Word
{-# INLINE wordArrayFromAddr #-}
wordArrayFromAddr l addr# =
#if SIZEOF_HSWORD == 8
    unsafeCoerce# (word64ArrayFromAddr l addr#)
#else
    unsafeCoerce# (word32ArrayFromAddr l addr#)
#endif

intArrayFromAddr :: Int -> Addr# -> PrimArray Int
{-# INLINE intArrayFromAddr #-}
intArrayFromAddr l addr# =
#if SIZEOF_HSWORD == 8
    unsafeCoerce# (int64ArrayFromAddr l addr#)
#else
    unsafeCoerce# (int32ArrayFromAddr l addr#)
#endif

ARRAY_LITERAL_DOC(Word)
wordLiteral :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
wordLiteral =
#if SIZEOF_HSWORD == 8
    word64Literal
#else
    word32Literal
#endif

ARRAY_LITERAL_DOC(Int)
intLiteral :: (ExpQ -> ExpQ -> ExpQ) -> String -> ExpQ
intLiteral =
#if SIZEOF_HSWORD == 8
    int64Literal
#else
    int32Literal
#endif

-- | @[arrWord|1,2,3,4,5|] :: PrimArray Word@
arrWord :: QuasiQuoter
arrWord = QuasiQuoter
    (wordLiteral $ \ len addr -> [| wordArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrWord as a pattern")
    (error "Cannot use arrWord as a type")
    (error "Cannot use arrWord as a dec")

-- | @[arrInt|1,2,3,4,5|] :: PrimArray Int@
arrInt :: QuasiQuoter
arrInt = QuasiQuoter
    (intLiteral $ \ len addr -> [| intArrayFromAddr $(len) $(addr) |])
    (error "Cannot use arrInt as a pattern")
    (error "Cannot use arrInt as a type")
    (error "Cannot use arrInt as a dec")

--------------------------------------------------------------------------------
