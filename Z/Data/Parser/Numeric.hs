{-|
Module      : Z.Data.Parser.Numeric
Description : Textual numeric parsers.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Textual numeric parsers.

-}

module Z.Data.Parser.Numeric
  ( -- * Decimal
    uint, int, integer
  , uint_, int_
    -- * Hex
  , hex, hex', hex_
    -- * Fractional
  , rational
  , float, double
  , scientific
  , scientifically
    -- * Stricter fractional(rfc8259)
  , rational'
  , float', double'
  , scientific'
  , scientifically'
    -- * Misc
  , hexLoop
  , decLoop
  , decLoopIntegerFast
  , isHexDigit
  , isDigit
  , floatToScientific
  , doubleToScientific
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Int
import qualified Data.Scientific          as Sci
import           Data.Word
import qualified Z.Data.Builder.Numeric as B
import           Z.Data.Parser.Base     (Parser, (<?>))
import qualified Z.Data.Parser.Base     as P
import qualified Z.Data.Vector.Base     as V
import qualified Z.Data.Vector.Extra    as V

#define WORD64_MAX_DIGITS_LEN 19
#define INT64_MAX_DIGITS_LEN 18

#define PLUS     43
#define MINUS    45
#define DOT      46
#define LITTLE_E 101
#define BIG_E    69
#define C_0 48

-- | Parse and decode an unsigned hex number, fail in case of overflow. The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string, and consider
-- sign bit part of the binary hex nibbles, e.g.
--
-- >>> parse' hex "FF" == Right (-1 :: Int8)
-- >>> parse' hex "7F" == Right (127 :: Int8)
-- >>> parse' hex "7Ft" == Right (127 :: Int8)
-- >>> parse' hex "7FF" == Left ["Z.Data.Parser.Numeric.hex","hex numeric number overflow"]
--
hex :: forall a.(Integral a, FiniteBits a) => Parser a
{-# INLINE hex #-}
hex = "Z.Data.Parser.Numeric.hex" <?> do
    bs <- P.takeWhile1 isHexDigit
    if V.length bs <= finiteBitSize (undefined :: a) `unsafeShiftR` 2
    then return (hexLoop 0 bs)
    else P.fail' "hex numeric number overflow"

-- | Same with 'hex', but only take as many as (bit_size/4) bytes.
--
-- >>> parse' hex "FF" == Right (-1 :: Int8)
-- >>> parse' hex "7F" == Right (127 :: Int8)
-- >>> parse' hex "7Ft" == Right (127 :: Int8)
-- >>> parse' hex "7FF" == Right (127 :: Int8)
hex' :: forall a.(Integral a, FiniteBits a) => Parser a
{-# INLINE hex' #-}
hex' = "Z.Data.Parser.Numeric.hex'" <?> do
    hexLoop 0 <$>
        P.takeN isHexDigit (finiteBitSize (undefined :: a) `unsafeShiftR` 2)
  where

-- | Same with 'hex', but silently cast in case of overflow.
--
-- >>> parse' hex "FF" == Right (-1 :: Int8)
-- >>> parse' hex "7F" == Right (127 :: Int8)
-- >>> parse' hex "7Ft" == Right (127 :: Int8)
-- >>> parse' hex "7FF" == Right (-1 :: Int8)
hex_ :: (Integral a, Bits a) => Parser a
{-# INLINE hex_ #-}
hex_ = "Z.Data.Parser.Numeric.hex_" <?> hexLoop 0 <$> P.takeWhile1 isHexDigit

-- | decode hex digits sequence within an array.
hexLoop :: (Integral a, Bits a)
        => a    -- ^ accumulator, usually start from 0
        -> V.Bytes
        -> a
{-# INLINE hexLoop #-}
hexLoop = V.foldl' step
  where
    step a w = a `unsafeShiftL` 4 + fromIntegral (w2iHex w)
    w2iHex w
        | w <= 57   = w - 48
        | w <= 70   = w - 55
        | w <= 102  = w - 87

-- | A fast digit predicate.
isHexDigit :: Word8 -> Bool
{-# INLINE isHexDigit #-}
isHexDigit w = w - 48 <= 9 || w - 65 <= 5 || w - 97 <= 5

-- | Same with 'uint', but sliently cast in case of overflow.
uint_ :: forall a. (Integral a, Bounded a) => Parser a
{-# INLINE uint_ #-}
uint_ = "Z.Data.Parser.Numeric.uint_" <?> decLoop 0 <$> P.takeWhile1 isDigit

-- | Parse and decode an unsigned decimal number.
--
-- Will fail in case of overflow.
uint :: forall a. (Integral a, Bounded a) => Parser a
{-# INLINE uint #-}
uint = "Z.Data.Parser.Numeric.uint" <?> do
    bs <- P.takeWhile1 isDigit
    if V.length bs <= WORD64_MAX_DIGITS_LEN
    then do
        let w64 = decLoop @Word64 0 bs
        if w64 <= fromIntegral (maxBound :: a)
        then return (fromIntegral w64)
        else P.fail' "decimal numeric value overflow"
    else do
        let w64 = decLoop @Integer 0 bs
        if w64 <= fromIntegral (maxBound :: a)
        then return (fromIntegral w64)
        else P.fail' "decimal numeric value overflow"

-- | Decode digits sequence within an array.
--
-- This function may overflow if result can't fit into type.
decLoop :: Integral a
        => a    -- ^ accumulator, usually start from 0
        -> V.Bytes
        -> a
{-# INLINE decLoop #-}
decLoop = V.foldl' step
  where step a w = a * 10 + fromIntegral (w - 48)

-- | decode digits sequence within an array.
--
-- A fast version to decode 'Integer' using machine word as much as possible.
decLoopIntegerFast :: V.Bytes -> Integer
{-# INLINE decLoopIntegerFast #-}
decLoopIntegerFast bs
    | V.length bs <= WORD64_MAX_DIGITS_LEN = fromIntegral (decLoop @Word64 0 bs)
    | otherwise                            = decLoop @Integer 0 bs

-- | A fast digit predicate.
isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

-- | Parse a decimal number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
--
-- This parser will fail if overflow happens.
int :: forall a. (Integral a, Bounded a) => Parser a
{-# INLINE int #-}
int = "Z.Data.Parser.Numeric.int" <?> do
    w <- P.peek
    if w == MINUS
    then P.skipWord8 *> loopNe
    else if w == PLUS then P.skipWord8 *> loop else loop
  where
    loop = do
        bs <- P.takeWhile1 isDigit
        if V.length bs <= WORD64_MAX_DIGITS_LEN
        then do
            let w64 = decLoop @Word64 0 bs
            if w64 <= fromIntegral (maxBound :: a)
            then return (fromIntegral w64)
            else P.fail' "decimal numeric value overflow"
        else do
            let w64 = decLoop @Integer 0 bs
            if w64 <= fromIntegral (maxBound :: a)
            then return (fromIntegral w64)
            else P.fail' "decimal numeric value overflow"
    loopNe = do
        bs <- P.takeWhile1 isDigit
        if V.length bs <= INT64_MAX_DIGITS_LEN
        then do
            let i64 = decLoop @Int64 0 bs
            if i64 <= negate (fromIntegral (minBound :: a))
            then return (fromIntegral (negate i64))
            else P.fail' "decimal numeric value overflow"
        else do
            let i64 = decLoop @Integer 0 bs
            if i64 <= negate (fromIntegral (minBound :: a))
            then return (fromIntegral (negate i64))
            else P.fail' "decimal numeric value overflow"

-- | Same with 'int', but sliently cast if overflow happens.
int_ :: (Integral a, Bounded a) => Parser a
{-# INLINE int_ #-}
int_ = "Z.Data.Parser.Numeric.int_" <?> do
    w <- P.peek
    if w == MINUS
    then P.skipWord8 *> (negate <$> loop)
    else if w == PLUS then P.skipWord8 *> loop else loop
  where
    loop = decLoop 0 <$> P.takeWhile1 isDigit

-- | Parser specifically optimized for 'Integer'.
--
integer :: Parser Integer
integer =  "Z.Data.Parser.Numeric.integer" <?> do
    w <- P.peek
    if w == MINUS
    then P.skipWord8 *> (negate <$> integer')
    else if w == PLUS then P.skipWord8 *> integer' else integer'
  where
    -- strip integer's message
    integer' = decLoopIntegerFast <$> P.takeWhile1 isDigit

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
-- /Note/: this parser is not safe for use with inputs from untrusted
-- sources.  An input with a suitably large exponent such as
-- @"1e1000000000"@ will cause a huge 'Integer' to be allocated,
-- resulting in what is effectively a denial-of-service attack.
--
-- In most cases, it is better to use 'double' or 'scientific'
-- instead.
--
rational :: (Fractional a) => Parser a
{-# INLINE rational #-}
rational = "Z.Data.Parser.Numeric.rational" <?> scientificallyInternal realToFrac

-- | Parse a rational number and round to 'Double'.
--
-- This parser accepts an optional leading sign character, followed by
-- at least one decimal digit.  The syntax similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ or
-- @\'e\'@ /not/ followed by a number is not consumed.
--
-- Examples with behaviour identical to 'read':
--
-- >parse' double "3"     == ("", Right 3.0)
-- >parse' double "3.1"   == ("", Right 3.1)
-- >parse' double "3e4"   == ("", Right 30000.0)
-- >parse' double "3.1e4" == ("", Right 31000.0)
--
-- >parse' double ".3"    == (".3", Left ParserError)
-- >parse' double "e3"    == ("e3", Left ParserError)
--
-- Examples of differences from 'read':
--
-- >parse' double "3.foo" == (".foo", Right 3.0)
-- >parse' double "3e"    == ("e",    Right 3.0)
-- >parse' double "-3e"   == ("e",    Right -3.0)
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
--
double :: Parser Double
{-# INLINE double #-}
double = "Z.Data.Parser.Numeric.double" <?> scientificallyInternal Sci.toRealFloat

-- | Parse a rational number and round to 'Float'.
--
-- Single precision version of 'double'.
float :: Parser Float
{-# INLINE float #-}
float = "Z.Data.Parser.Numeric.float" <?> scientificallyInternal Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
scientific :: Parser Sci.Scientific
{-# INLINE scientific #-}
scientific = "Z.Data.Parser.Numeric.scientific" <?> scientificallyInternal id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double'.
scientifically :: (Sci.Scientific -> a) -> Parser a
{-# INLINE scientifically #-}
scientifically h = "Z.Data.Parser.Numeric.scientifically" <?> scientificallyInternal h

-- | Strip message version.
scientificallyInternal :: (Sci.Scientific -> a) -> Parser a
{-# INLINE scientificallyInternal #-}
scientificallyInternal h = do
    !sign <- P.peek
    when (sign == PLUS || sign == MINUS) (P.skipWord8)
    !intPart <- P.takeWhile1 isDigit
    -- backtrack here is neccessary to avoid eating extra dot or e
    -- attoparsec is doing it wrong here: https://github.com/bos/attoparsec/issues/112
    !sci <- (do
        -- during number parsing we want to use machine word as much as possible
        -- so as long as range permit, we use Word64 instead of final Integer
        !fracPart <- P.word8 DOT *> P.takeWhile1 isDigit
        let !ilen = V.length intPart
            !flen = V.length fracPart
            !base =
                if ilen + flen <= WORD64_MAX_DIGITS_LEN
                then fromIntegral (decLoop @Word64 (decLoop @Word64 0 intPart) fracPart)
                else
                    let i = decLoopIntegerFast intPart
                        f = decLoopIntegerFast fracPart
                    in i * 10 ^ flen + f
        parseE base flen) <|> (parseE (decLoopIntegerFast intPart) 0)

    pure $! if sign /= MINUS then h sci else h (negate sci)
  where
    {-# INLINE parseE #-}
    parseE c e =
        (do _ <- P.satisfy (\w -> w ==  LITTLE_E || w == BIG_E)
            Sci.scientific c . subtract e <$> int) <|> pure (Sci.scientific c (negate e))

--------------------------------------------------------------------------------

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'double''.
--
-- /Note/: this parser is not safe for use with inputs from untrusted
-- sources.  An input with a suitably large exponent such as
-- @"1e1000000000"@ will cause a huge 'Integer' to be allocated,
-- resulting in what is effectively a denial-of-service attack.
--
-- In most cases, it is better to use 'double'' or 'scientific''
-- instead.
--
rational' :: (Fractional a) => Parser a
{-# INLINE rational' #-}
rational' = "Z.Data.Parser.Numeric.rational'" <?> scientificallyInternal' realToFrac

-- | More strict number parsing(rfc8259).
--
-- 'scientific' support parse @2314.@ and @21321exyz@ without eating extra dot or @e@ via
-- backtrack, this is not allowed in some strict grammer such as JSON, so we make an
-- non-backtrack strict number parser separately using LL(1) lookahead. This parser also
-- agree with 'read' on extra dot or e handling:
--
-- >parse' double "3.foo" == Left ParseError
-- >parse' double "3e"    == Left ParseError
--
-- Leading zeros or @+@ sign is also not allowed:
--
-- >parse' double "+3.14" == Left ParseError
-- >parse' double "0014" == Left ParseError
--
-- If you have a similar grammer, you can use this parser to save considerable time.
--
-- @
--      number = [ minus ] int [ frac ] [ exp ]
--      decimal-point = %x2E       ; .
--      digit1-9 = %x31-39         ; 1-9
--      e = %x65 / %x45            ; e E
--      exp = e [ minus / plus ] 1*DIGIT
--      frac = decimal-point 1*DIGIT
-- @
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
-- reference: https://tools.ietf.org/html/rfc8259#section-6
double' :: Parser Double
{-# INLINE double' #-}
double' = "Z.Data.Parser.Numeric.double'" <?> scientificallyInternal' Sci.toRealFloat

-- | Parse a rational number and round to 'Float' using stricter grammer.
--
-- Single precision version of 'double''.
float' :: Parser Float
{-# INLINE float' #-}
float' = "Z.Data.Parser.Numeric.float'" <?> scientificallyInternal' Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double''.
scientific' :: Parser Sci.Scientific
{-# INLINE scientific' #-}
scientific' = "Z.Data.Parser.Numeric.scientific'" <?> scientificallyInternal' id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double''.
scientifically' :: (Sci.Scientific -> a) -> P.Parser a
{-# INLINE scientifically' #-}
scientifically' h = "Z.Data.Parser.Numeric.scientifically'" <?> scientificallyInternal' h

-- | Strip message version of scientifically'.
scientificallyInternal' :: (Sci.Scientific -> a) -> P.Parser a
{-# INLINE scientificallyInternal' #-}
scientificallyInternal' h = do
    !sign <- P.peek
    when (sign == MINUS) (P.skipWord8) -- no leading plus is allowed
    !intPart <- P.takeWhile1 isDigit
    when (V.length intPart > 1 && V.head intPart == C_0) (P.fail' "leading zeros are not allowed")
    mdot <- P.peekMaybe
    !sci <- case mdot of
        Just DOT -> do
            !fracPart <- P.skipWord8 *> P.takeWhile1 isDigit
            -- during number parsing we want to use machine word as much as possible
            -- so as long as range permit, we use Word64 instead of final Integer
            let !ilen = V.length intPart
                !flen = V.length fracPart
                !base =
                    if ilen + flen <= WORD64_MAX_DIGITS_LEN
                    then fromIntegral (decLoop @Word64 (decLoop @Word64 0 intPart) fracPart)
                    else
                        let i = decLoopIntegerFast intPart
                            f = decLoopIntegerFast fracPart
                        in i * 10 ^ flen + f
            parseE base flen
        _ -> parseE (decLoopIntegerFast intPart) 0
    pure $! if sign /= MINUS then h sci else h (negate sci)
  where
    {-# INLINE parseE #-}
    parseE !c !e = do
        me <- P.peekMaybe
        e' <- case me of
            Just ec | ec == LITTLE_E || ec == BIG_E -> P.skipWord8 *> int
            _ -> pure 0
        pure $! Sci.scientific c (e' - e)

--------------------------------------------------------------------------------

floatToScientific :: Float -> Sci.Scientific
{-# INLINE floatToScientific #-}
floatToScientific rf | rf < 0    = -(fromFloatingDigits (B.grisu3_sp (-rf)))
                     | rf == 0   = 0
                     | otherwise = fromFloatingDigits (B.grisu3_sp rf)

doubleToScientific :: Double -> Sci.Scientific
{-# INLINE doubleToScientific #-}
doubleToScientific rf | rf < 0    = -(fromFloatingDigits (B.grisu3 (-rf)))
                      | rf == 0   = 0
                      | otherwise = fromFloatingDigits (B.grisu3 rf)

fromFloatingDigits :: ([Int], Int) -> Sci.Scientific
{-# INLINE fromFloatingDigits #-}
fromFloatingDigits (digits, e) = go digits 0 0
  where
    -- There's no way a float or double has more digits a 'Int64' can't handle
    go :: [Int] -> Int64 -> Int -> Sci.Scientific
    go []     !c !n = Sci.scientific (fromIntegral c) (e - n)
    go (d:ds) !c !n = go ds (c * 10 + fromIntegral d) (n + 1)
