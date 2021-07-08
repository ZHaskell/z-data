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
  , uint_, int_, digit
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
  , w2iHex, w2iDec
  , hexLoop
  , decLoop
  , decLoopIntegerFast
  , sciToDouble
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Int
import qualified Data.Scientific        as Sci
import           Data.Word
#ifdef INTEGER_GMP
import           GHC.Integer.GMP.Internals
#endif
import           GHC.Exts
import           GHC.Float              (expt)
import           Z.Data.ASCII
import           Z.Data.Parser.Base     (Parser, (<?>))
import qualified Z.Data.Parser.Base     as P
import qualified Z.Data.Vector.Base     as V
import qualified Z.Data.Vector.Extra    as V
import           Z.Foreign
import           System.IO.Unsafe

#define WORD64_SAFE_DIGITS_LEN 19
#define INT64_SAFE_DIGITS_LEN 18


-- | Parse and decode an unsigned hex number, fail if input length is larger than (bit_size/4). The hex digits
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
{-# INLINABLE hex #-}
{-# SPECIALIZE INLINE hex :: Parser Int #-}
{-# SPECIALIZE INLINE hex :: Parser Int8 #-}
{-# SPECIALIZE INLINE hex :: Parser Int16 #-}
{-# SPECIALIZE INLINE hex :: Parser Int32 #-}
{-# SPECIALIZE INLINE hex :: Parser Int64 #-}
{-# SPECIALIZE INLINE hex :: Parser Word #-}
{-# SPECIALIZE INLINE hex :: Parser Word8 #-}
{-# SPECIALIZE INLINE hex :: Parser Word16 #-}
{-# SPECIALIZE INLINE hex :: Parser Word32 #-}
{-# SPECIALIZE INLINE hex :: Parser Word64 #-}
hex = "Z.Data.Parser.Numeric.hex" <?> do
    bs <- P.takeWhile1 isHexDigit
    if V.length bs <= finiteBitSize (undefined :: a) `unsafeShiftR` 2
    then return $! hexLoop 0 bs
    else P.fail' "hex numeric number overflow"

-- | Same with 'hex', but only take as many as (bit_size/4) bytes.
--
-- >>> parse' hex "FF" == Right (-1 :: Int8)
-- >>> parse' hex "7F" == Right (127 :: Int8)
-- >>> parse' hex "7Ft" == Right (127 :: Int8)
-- >>> parse' hex "7FF" == Right (127 :: Int8)
hex' :: forall a.(Integral a, FiniteBits a) => Parser a
{-# INLINABLE hex' #-}
{-# SPECIALIZE INLINE hex' :: Parser Int #-}
{-# SPECIALIZE INLINE hex' :: Parser Int8 #-}
{-# SPECIALIZE INLINE hex' :: Parser Int16 #-}
{-# SPECIALIZE INLINE hex' :: Parser Int32 #-}
{-# SPECIALIZE INLINE hex' :: Parser Int64 #-}
{-# SPECIALIZE INLINE hex' :: Parser Word #-}
{-# SPECIALIZE INLINE hex' :: Parser Word8 #-}
{-# SPECIALIZE INLINE hex' :: Parser Word16 #-}
{-# SPECIALIZE INLINE hex' :: Parser Word32 #-}
{-# SPECIALIZE INLINE hex' :: Parser Word64 #-}
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
{-# INLINABLE hex_ #-}
{-# SPECIALIZE INLINE hex_ :: Parser Int #-}
{-# SPECIALIZE INLINE hex_ :: Parser Int8 #-}
{-# SPECIALIZE INLINE hex_ :: Parser Int16 #-}
{-# SPECIALIZE INLINE hex_ :: Parser Int32 #-}
{-# SPECIALIZE INLINE hex_ :: Parser Int64 #-}
{-# SPECIALIZE INLINE hex_ :: Parser Word #-}
{-# SPECIALIZE INLINE hex_ :: Parser Word8 #-}
{-# SPECIALIZE INLINE hex_ :: Parser Word16 #-}
{-# SPECIALIZE INLINE hex_ :: Parser Word32 #-}
{-# SPECIALIZE INLINE hex_ :: Parser Word64 #-}
hex_ = "Z.Data.Parser.Numeric.hex_" <?> hexLoop 0 <$> P.takeWhile1 isHexDigit

-- | decode hex digits sequence within an array.
hexLoop :: forall a. (Integral a, Bits a)
        => a    -- ^ accumulator, usually start from 0
        -> V.Bytes
        -> a
{-# INLINE hexLoop #-}
hexLoop = V.foldl' step
  where
    step a w = a `unsafeShiftL` 4 + fromIntegral (w2iHex w :: a)

-- | Convert A ASCII hex digit to 'Int' value.
w2iHex :: Integral a => Word8 -> a
{-# INLINE w2iHex #-}
w2iHex w
    | w <= 57   = fromIntegral w - 48
    | w <= 70   = fromIntegral w - 55
    | otherwise = fromIntegral w - 87

-- | Same with 'uint', but sliently cast in case of overflow.
uint_ :: forall a. (Integral a, Bounded a) => Parser a
{-# INLINABLE uint_ #-}
{-# SPECIALIZE INLINE uint_ :: Parser Int #-}
{-# SPECIALIZE INLINE uint_ :: Parser Int8 #-}
{-# SPECIALIZE INLINE uint_ :: Parser Int16 #-}
{-# SPECIALIZE INLINE uint_ :: Parser Int32 #-}
{-# SPECIALIZE INLINE uint_ :: Parser Int64 #-}
{-# SPECIALIZE INLINE uint_ :: Parser Word #-}
{-# SPECIALIZE INLINE uint_ :: Parser Word8 #-}
{-# SPECIALIZE INLINE uint_ :: Parser Word16 #-}
{-# SPECIALIZE INLINE uint_ :: Parser Word32 #-}
{-# SPECIALIZE INLINE uint_ :: Parser Word64 #-}
uint_ = "Z.Data.Parser.Numeric.uint_" <?> decLoop 0 <$> P.takeWhile1 isDigit

-- | Parse and decode an unsigned decimal number.
--
-- Will fail in case of overflow.
uint :: forall a. (Integral a, Bounded a) => Parser a
{-# INLINABLE uint #-}
{-# SPECIALIZE INLINE uint :: Parser Int #-}
{-# SPECIALIZE INLINE uint :: Parser Int8 #-}
{-# SPECIALIZE INLINE uint :: Parser Int16 #-}
{-# SPECIALIZE INLINE uint :: Parser Int32 #-}
{-# SPECIALIZE INLINE uint :: Parser Int64 #-}
{-# SPECIALIZE INLINE uint :: Parser Word #-}
{-# SPECIALIZE INLINE uint :: Parser Word8 #-}
{-# SPECIALIZE INLINE uint :: Parser Word16 #-}
{-# SPECIALIZE INLINE uint :: Parser Word32 #-}
{-# SPECIALIZE INLINE uint :: Parser Word64 #-}
uint = "Z.Data.Parser.Numeric.uint" <?> do
    bs <- P.takeWhile1 isDigit
    if V.length bs <= WORD64_SAFE_DIGITS_LEN
    then do
        let w64 = decLoop @Word64 0 bs
        if w64 <= fromIntegral (maxBound :: a)
        then return $! fromIntegral w64
        else P.fail' "decimal numeric value overflow"
    else do
        let w64 = decLoop @Integer 0 bs
        if w64 <= fromIntegral (maxBound :: a)
        then return $! fromIntegral w64
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
  where step a w = a * 10 + w2iDec w


-- | Convert A ASCII decimal digit to 'Int' value.
--
w2iDec :: Integral a => Word8 -> a
{-# INLINE w2iDec #-}
w2iDec w = fromIntegral w - 48

-- | decode digits sequence within an array.
--
-- A fast version to decode 'Integer' using machine word as much as possible.
decLoopIntegerFast :: V.Bytes -> Integer
{-# INLINE decLoopIntegerFast #-}
decLoopIntegerFast bs
    | V.length bs <= WORD64_SAFE_DIGITS_LEN = fromIntegral (decLoop @Word64 0 bs)
    | otherwise                            = decLoop @Integer 0 bs


-- | Take a single decimal digit and return as 'Int'.
--
digit :: Parser Int
{-# INLINE digit #-}
digit = do
    d <- P.satisfy isDigit
    return $! w2iDec d

-- | Parse a decimal number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
--
-- This parser will fail if overflow happens.
int :: forall a. (Integral a, Bounded a) => Parser a
{-# INLINABLE int #-}
{-# SPECIALIZE INLINE int :: Parser Int #-}
{-# SPECIALIZE INLINE int :: Parser Int8 #-}
{-# SPECIALIZE INLINE int :: Parser Int16 #-}
{-# SPECIALIZE INLINE int :: Parser Int32 #-}
{-# SPECIALIZE INLINE int :: Parser Int64 #-}
{-# SPECIALIZE INLINE int :: Parser Word #-}
{-# SPECIALIZE INLINE int :: Parser Word8 #-}
{-# SPECIALIZE INLINE int :: Parser Word16 #-}
{-# SPECIALIZE INLINE int :: Parser Word32 #-}
{-# SPECIALIZE INLINE int :: Parser Word64 #-}
int = "Z.Data.Parser.Numeric.int" <?> do
    w <- P.peek
    if w == MINUS
    then P.skipWord8 *> loopNe
    else if w == PLUS then P.skipWord8 *> loop else loop
  where
    loop = do
        bs <- P.takeWhile1 isDigit
        if V.length bs <= WORD64_SAFE_DIGITS_LEN
        then do
            let w64 = decLoop @Word64 0 bs
            if w64 <= fromIntegral (maxBound :: a)
            then return $! fromIntegral w64
            else P.fail' "decimal numeric value overflow"
        else do
            let w64 = decLoop @Integer 0 bs
            if w64 <= fromIntegral (maxBound :: a)
            then return $! fromIntegral w64
            else P.fail' "decimal numeric value overflow"
    loopNe = do
        bs <- P.takeWhile1 isDigit
        if V.length bs <= INT64_SAFE_DIGITS_LEN
        then do
            let i64 = negate (decLoop @Int64 0 bs)
            if i64 >= fromIntegral (minBound :: a)
            then return $! fromIntegral i64
            else P.fail' "decimal numeric value overflow"
        else do
            let i64 = negate (decLoop @Integer 0 bs)
            if i64 >= fromIntegral (minBound :: a)
            then return $! fromIntegral i64
            else P.fail' "decimal numeric value overflow"

-- | Same with 'int', but sliently cast if overflow happens.
int_ :: (Integral a, Bounded a) => Parser a
{-# INLINABLE int_ #-}
{-# SPECIALIZE INLINE int_ :: Parser Int #-}
{-# SPECIALIZE INLINE int_ :: Parser Int8 #-}
{-# SPECIALIZE INLINE int_ :: Parser Int16 #-}
{-# SPECIALIZE INLINE int_ :: Parser Int32 #-}
{-# SPECIALIZE INLINE int_ :: Parser Int64 #-}
{-# SPECIALIZE INLINE int_ :: Parser Word #-}
{-# SPECIALIZE INLINE int_ :: Parser Word8 #-}
{-# SPECIALIZE INLINE int_ :: Parser Word16 #-}
{-# SPECIALIZE INLINE int_ :: Parser Word32 #-}
{-# SPECIALIZE INLINE int_ :: Parser Word64 #-}
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
{-# INLINABLE integer #-}
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
{-# INLINABLE rational #-}
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
{-# INLINABLE double #-}
double = "Z.Data.Parser.Numeric.double" <?> scientificallyInternal sciToDouble

-- | Parse a rational number and round to 'Float'.
--
-- Single precision version of 'double'.
float :: Parser Float
{-# INLINABLE float #-}
float = "Z.Data.Parser.Numeric.float" <?> scientificallyInternal Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
scientific :: Parser Sci.Scientific
{-# INLINABLE scientific #-}
scientific = "Z.Data.Parser.Numeric.scientific" <?> scientificallyInternal id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double'.
scientifically :: (Sci.Scientific -> a) -> Parser a
{-# INLINABLE scientifically #-}
scientifically h = "Z.Data.Parser.Numeric.scientifically" <?> scientificallyInternal h

-- | Strip message version.
scientificallyInternal :: (Sci.Scientific -> a) -> Parser a
{-# INLINABLE scientificallyInternal #-}
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
                if ilen + flen <= WORD64_SAFE_DIGITS_LEN
                then fromIntegral (decLoop @Word64 (decLoop @Word64 0 intPart) fracPart)
                else
                    let i = decLoopIntegerFast intPart
                        f = decLoopIntegerFast fracPart
                    in i * (expt 10 flen) + f
        parseE base flen) <|> (parseE (decLoopIntegerFast intPart) 0)
    -- intentionally lazy return here, we have done the grammar check, and h could potentially be very expensive, e.g. sciToDouble
    -- retained references are sign and sci, which are already in NF
    pure (if sign /= MINUS then h sci else h (negate sci))
  where
    {-# INLINE parseE #-}
    parseE c e =
        (do _ <- P.satisfy (\w -> w ==  LETTER_e || w == LETTER_E)
            e' <- int
            pure $! Sci.scientific c (e' - e)) <|> (pure $! Sci.scientific c (negate e))

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
{-# INLINABLE rational' #-}
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
{-# INLINABLE double' #-}
double' = "Z.Data.Parser.Numeric.double'" <?> scientificallyInternal' sciToDouble

#define FASTFLOAT_SMALLEST_POWER -325
#define FASTFLOAT_LARGEST_POWER 308

-- | Faster scientific to double conversion using <https://github.com/lemire/fast_double_parser/>.
--
-- See @cbits/compute_float_64.c@.
sciToDouble :: Sci.Scientific -> Double
{-# INLINABLE sciToDouble #-}
sciToDouble sci = case c of
#ifdef INTEGER_GMP
    (S# i#) | (e >= FASTFLOAT_SMALLEST_POWER && e <= FASTFLOAT_LARGEST_POWER) -> unsafeDupablePerformIO $ do
        let i = (I# i#)
            s = if i >= 0 then 0 else 1
            i' = fromIntegral $ if i >= 0 then i else (0-i)
        (success, r) <- allocPrimUnsafe @Word8 (compute_float_64 (fromIntegral e) i' s)
        if success == 0
        then return $! Sci.toRealFloat sci
        else return $! r
#endif
    _ -> Sci.toRealFloat sci
  where
    e = Sci.base10Exponent sci
    c = Sci.coefficient sci

-- | Parse a rational number and round to 'Float' using stricter grammer.
--
-- Single precision version of 'double''.
float' :: Parser Float
{-# INLINABLE float' #-}
float' = "Z.Data.Parser.Numeric.float'" <?> scientificallyInternal' Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double''.
scientific' :: Parser Sci.Scientific
{-# INLINABLE scientific' #-}
scientific' = "Z.Data.Parser.Numeric.scientific'" <?> scientificallyInternal' id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double''.
scientifically' :: (Sci.Scientific -> a) -> P.Parser a
{-# INLINABLE scientifically' #-}
scientifically' h = "Z.Data.Parser.Numeric.scientifically'" <?> scientificallyInternal' h

-- | Strip message version of scientifically'.
scientificallyInternal' :: (Sci.Scientific -> a) -> P.Parser a
{-# INLINABLE scientificallyInternal' #-}
scientificallyInternal' h = do
    !sign <- P.peek
    when (sign == MINUS) (P.skipWord8) -- no leading plus is allowed
    !intPart <- P.takeWhile1 isDigit
    when (V.length intPart > 1 && V.head intPart == DIGIT_0) (P.fail' "leading zeros are not allowed")
    mdot <- P.peekMaybe
    !sci <- case mdot of
        Just DOT -> do
            !fracPart <- P.skipWord8 *> P.takeWhile1 isDigit
            -- during number parsing we want to use machine word as much as possible
            -- so as long as range permit, we use Word64 instead of final Integer
            let !ilen = V.length intPart
                !flen = V.length fracPart
                !base =
                    if ilen + flen <= WORD64_SAFE_DIGITS_LEN
                    then fromIntegral (decLoop @Word64 (decLoop @Word64 0 intPart) fracPart)
                    else
                        let i = decLoopIntegerFast intPart
                            f = decLoopIntegerFast fracPart
                        in i * (expt 10 flen) + f
            parseE base flen
        _ -> parseE (decLoopIntegerFast intPart) 0
    -- intentionally lazy return here, we have done the grammar check, and h could potentially be very expensive, e.g. sciToDouble
    -- retained references are sign and sci, which are already in NF
    pure (if sign /= MINUS then h sci else h (negate sci))
  where
    {-# INLINE parseE #-}
    parseE !c !e = do
        me <- P.peekMaybe
        e' <- case me of
            Just ec | ec == LETTER_e || ec == LETTER_E -> P.skipWord8 *> int
            _ -> pure 0
        pure $! Sci.scientific c (e' - e)

foreign import ccall unsafe compute_float_64 :: Int64   -- ^ power of 10
                                             -> Word64  -- ^ base
                                             -> Word8   -- ^ negative
                                             -> MBA# Word8      -- ^ success?
                                             -> IO Double       -- ^ result
