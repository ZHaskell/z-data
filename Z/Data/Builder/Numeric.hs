{-|
Module      : Z.Data.Builder.Numeric
Description : Textual numeric builders.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Textual numeric builders.

-}

module Z.Data.Builder.Numeric (
  -- * Integral type formatting

    IFormat(..)
  , defaultIFormat
  , Padding(..)
  , int
  , intWith
  , integer
  -- * Fixded size hexidecimal formatting
  , hex, hexUpper
  -- * IEEE float formating
  , FFormat(..)
  , double
  , doubleWith
  , float
  , floatWith
  , scientific
  , scientific'
  , scientificWith
  -- * Misc
  , grisu3
  , grisu3_sp
  , i2wDec, i2wHex, i2wHexUpper
  , countDigits
  , c_intWith, hs_intWith
  , quotRem10
) where

import           Control.Monad
import           Data.Bits
import           Data.Int
import qualified Data.List                           as List
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import qualified Data.Scientific                     as Sci
import           Data.Word
import           GHC.Exts
import           GHC.Float
import           GHC.Num
import           Z.Data.ASCII
import           Z.Data.Builder.Base
import           Z.Data.Builder.Numeric.DigitTable
import           Z.Foreign
import           System.IO.Unsafe
import           Test.QuickCheck.Arbitrary           (Arbitrary(..), CoArbitrary(..))
import           Z.MonoList.IntList                  (IntList(..))
import qualified Z.MonoList.IntList                  as IntList

--------------------------------------------------------------------------------

foreign import ccall unsafe "dtoa.h"
    c_int_dec :: Word64 -> Int -> Int -> Word8 -> MBA# Word8 -> Int -> IO Int

-- | Integral formatting options.
--
data IFormat = IFormat
    { width       :: Int            -- ^ total width, only effective with padding options
    , padding     :: Padding        -- ^ padding options
    , posSign     :: Bool           -- ^ show @+@ when the number is positive
    } deriving (Show, Eq, Ord)

instance Arbitrary IFormat where
    arbitrary = IFormat <$> arbitrary <*> arbitrary <*> arbitrary

instance CoArbitrary IFormat where
    coarbitrary (IFormat w pad p) = coarbitrary (w, pad, p)


-- | @defaultIFormat = IFormat 0 NoPadding False@
defaultIFormat :: IFormat
{-# INLINE defaultIFormat #-}
defaultIFormat = IFormat 0 NoPadding False

-- | Padding format.
data Padding = NoPadding | RightSpacePadding | LeftSpacePadding | ZeroPadding deriving (Show, Eq, Ord, Enum)

instance Arbitrary Padding where
    arbitrary = toEnum . (`mod` 4) <$> arbitrary

instance CoArbitrary Padding where
    coarbitrary = coarbitrary . fromEnum

-- | @int = intWith defaultIFormat@
int :: (Integral a, Bounded a) => a -> Builder ()
{-# INLINE int #-}
int = intWith defaultIFormat

-- | Format a 'Bounded' 'Integral' type like @Int@ or @Word16@ into decimal ASCII digits.
--
-- @
-- import Z.Data.Builder as B
--
-- > B.buildText $ B.intWith defaultIFormat  (12345 :: Int)
-- "12345"
-- > B.buildText $ B.intWith defaultIFormat{width=10, padding=RightSpacePadding} (12345 :: Int)
-- "12345     "
-- > B.buildText $ B.intWith defaultIFormat{width=10, padding=ZeroPadding} (12345 :: Int)
-- "0000012345"
-- @
--
intWith :: (Integral a, Bounded a) => IFormat -> a -> Builder ()
intWith = hs_intWith
{-# INLINE [1] intWith #-}
{-# RULES "intWith'/Int"     intWith = c_intWith  :: IFormat -> Int     -> Builder () #-}
{-# RULES "intWith'/Int8"    intWith = c_intWith  :: IFormat -> Int8    -> Builder () #-}
{-# RULES "intWith'/Int16"   intWith = c_intWith  :: IFormat -> Int16   -> Builder () #-}
{-# RULES "intWith'/Int32"   intWith = c_intWith  :: IFormat -> Int32   -> Builder () #-}
{-# RULES "intWith'/Int64"   intWith = c_intWith  :: IFormat -> Int64   -> Builder () #-}
{-# RULES "intWith'/Word"    intWith = c_intWith  :: IFormat -> Word    -> Builder () #-}
{-# RULES "intWith'/Word8"   intWith = c_intWith  :: IFormat -> Word8   -> Builder () #-}
{-# RULES "intWith'/Word16"  intWith = c_intWith  :: IFormat -> Word16  -> Builder () #-}
{-# RULES "intWith'/Word32"  intWith = c_intWith  :: IFormat -> Word32  -> Builder () #-}
{-# RULES "intWith'/Word64"  intWith = c_intWith  :: IFormat -> Word64  -> Builder () #-}
{-# RULES "intWith'/CShort"  intWith = c_intWith  :: IFormat -> CShort  -> Builder () #-}
{-# RULES "intWith'/CUShort" intWith = c_intWith  :: IFormat -> CUShort -> Builder () #-}
{-# RULES "intWith'/CInt"    intWith = c_intWith  :: IFormat -> CInt    -> Builder () #-}
{-# RULES "intWith'/CUInt"   intWith = c_intWith  :: IFormat -> CUInt   -> Builder () #-}
{-# RULES "intWith'/CLong"   intWith = c_intWith  :: IFormat -> CLong   -> Builder () #-}
{-# RULES "intWith'/CULong"  intWith = c_intWith  :: IFormat -> CULong  -> Builder () #-}
{-# RULES "intWith'/CLLong"  intWith = c_intWith  :: IFormat -> CLLong  -> Builder () #-}
{-# RULES "intWith'/CULLong" intWith = c_intWith  :: IFormat -> CULLong -> Builder () #-}

-- | Internal formatting backed by C FFI, it must be used with type smaller than 'Word64'.
--
-- We use rewrite rules to rewrite most of the integral types formatting to this function.
c_intWith :: (Integral a, Bits a) => IFormat -> a -> Builder ()
{-# INLINE c_intWith #-}
c_intWith (IFormat{..}) = \ x ->
    ensureN (max 21 width) (\ (MutablePrimArray mba#) i ->
        if x < 0
        then let !x' = (fromIntegral (complement x) :: Word64) + 1
             in (c_int_dec x' (-1) width pad mba# i)
        else c_int_dec (fromIntegral x) (if posSign then 1 else 0) width pad mba# i)
  where
    pad = case padding of NoPadding          -> 0
                          RightSpacePadding  -> 1
                          LeftSpacePadding   -> 2
                          _                  -> 3

-- | Internal formatting in haskell, it can be used with any bounded integral type.
--
-- Other than provide fallback for the c version, this function is also used to check
-- the c version's formatting result.
hs_intWith :: (Integral a, Bounded a) => IFormat -> a -> Builder ()
{-# INLINABLE hs_intWith #-}
hs_intWith format@IFormat{..} i
    | i < 0 =
        if i == minBound            -- can't directly negate in this case
        then do
            let (q, r) = i `quotRem` 10
                !qq = -q            -- all digits except last one
                !rr = i2wDec (-r)      -- last digits
                !n = countDigits qq
                !n' = n + 2         -- extra two bytes: MINUS and last digit
            if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off MINUS                       -- leading MINUS
                        let off' = off + 1
                        writePositiveDec marr off' n qq                      -- digits
                        let off'' = off' + n
                        writePrimArray marr off'' rr                        -- last digit
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off MINUS                   -- leading MINUS
                        let off' = off + 1
                        setPrimArray marr off' leadingN DIGIT_0            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveDec marr off'' n qq                 -- digits
                        let off''' = off'' + n
                        writePrimArray marr off''' rr                   -- last digit
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN SPACE            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' MINUS                  -- leading MINUS
                        let off'' = off' + 1
                        writePositiveDec marr off'' n qq                 -- digits
                        let off''' = off'' + n
                        writePrimArray marr off''' rr                   -- last digit
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off MINUS                   -- leading MINUS
                        let off' = off + 1
                        writePositiveDec marr off' n qq                  -- digits
                        let off'' = off' + n
                        writePrimArray marr off'' rr                    -- last digit
                        let off''' = off'' + 1
                        setPrimArray marr off''' trailingN SPACE        -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off MINUS                       -- leading MINUS
                    let off' = off + 1
                    writePositiveDec marr off' n qq                      -- digits
                    let off'' = off' + n
                    writePrimArray marr off'' rr                        -- last digit
        else do
            let !qq = -i
                !n = countDigits qq
                !n' = n + 1  -- extra byte: MINUS
            if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off MINUS                       -- leading MINUS
                        let off' = off + 1
                        writePositiveDec marr off' n qq                      -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off MINUS                   -- leading MINUS
                        let off' = off + 1
                        setPrimArray marr off' leadingN DIGIT_0            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveDec marr off'' n qq                 -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN SPACE            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' MINUS                  -- leading MINUS
                        let off'' = off' + 1
                        writePositiveDec marr off'' n qq                 -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off MINUS                   -- leading MINUS
                        let off' = off + 1
                        writePositiveDec marr off' n qq                  -- digits
                        let off'' = off' + n
                        setPrimArray marr off'' trailingN SPACE         -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off MINUS                       -- leading MINUS
                    let off' = off + 1
                    writePositiveDec marr off' n qq                      -- digits
    | otherwise = positiveInt format i

positiveInt :: (Integral a) => IFormat -> a -> Builder ()
{-# INLINABLE positiveInt #-}
positiveInt (IFormat width padding ps) i =
    let !n = countDigits i
    in if ps
        then
            let n' = n+1
            in if width > n'
            then case padding of
                NoPadding ->
                    writeN n' $ \marr off -> do
                        writePrimArray marr off PLUS                    -- leading PLUS
                        let off' = off + 1
                        writePositiveDec marr off' n i                   -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        writePrimArray marr off PLUS                    -- leading PLUS
                        let off' = off + 1
                        setPrimArray marr off' leadingN DIGIT_0            -- leading zeros
                        let off'' = off' + leadingN
                        writePositiveDec marr off'' n i                  -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n'
                        setPrimArray marr off leadingN SPACE            -- leading spaces
                        let off' = off + leadingN
                        writePrimArray marr off' PLUS                   -- leading PLUS
                        let off'' = off' + 1
                        writePositiveDec marr off'' n i                  -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n'
                        writePrimArray marr off PLUS                    -- leading PLUS
                        let off' = off + 1
                        writePositiveDec marr off' n i                   -- digits
                        let off'' = off' + n
                        setPrimArray marr off'' trailingN SPACE         -- trailing spaces
            else
                writeN n' $ \marr off -> do
                    writePrimArray marr off PLUS                        -- leading PLUS
                    let off' = off + 1
                    writePositiveDec marr off' n i                       -- digits

        else if width > n
            then case padding of
                NoPadding ->
                    writeN n $ \marr off -> do
                        writePositiveDec marr off n i                    -- digits
                ZeroPadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN DIGIT_0             -- leading zeros
                        let off' = off + leadingN
                        writePositiveDec marr off' n i                   -- digits
                LeftSpacePadding ->
                    writeN width $ \marr off -> do
                        let !leadingN = width-n
                        setPrimArray marr off leadingN SPACE            -- leading spaces
                        let off' = off + leadingN
                        writePositiveDec marr off' n i                   -- digits
                RightSpacePadding ->
                    writeN width $ \marr off -> do
                        let !trailingN = width-n
                        writePositiveDec marr off n i                    -- digits
                        let off' = off + n
                        setPrimArray marr off' trailingN SPACE          -- trailing spaces
            else
                writeN n $ \marr off -> do
                    writePositiveDec marr off n i                        -- digits

writePositiveDec :: (Integral a)
                => MutablePrimArray RealWorld Word8       -- ^ The buffer
                -> Int                                      -- ^ writing offset
                -> Int                                      -- ^ total digits
                -> a                                        -- ^ the value
                -> IO ()
{-# INLINE writePositiveDec #-}
writePositiveDec marr off0 ds = go (off0 + ds - 1)
  where
    go off v
        | v >= 100 = do
            let (q, r) = v `quotRem` 100
            write2 off r
            go (off - 2) q
        | v < 10    = writePrimArray marr off (i2wDec v)
        | otherwise = write2 off v
    write2 off i0 = do
        let i = fromIntegral i0;
        writePrimWord8ArrayAs marr (off-1) $ indexOffPtr decDigitTable i


--------------------------------------------------------------------------------
-- Below is an implementation of formatting integer, the main
-- idea is borrowed from base (GHC.Show).

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define DIGITS       9
#define BASE         1000000000
#elif SIZEOF_HSWORD == 8
#define DIGITS       18
#define BASE         1000000000000000000
#else
#error Please define DIGITS and BASE
-- DIGITS should be the largest integer such that
--     10^DIGITS < 2^(SIZEOF_HSWORD * 8 - 1)
-- BASE should be 10^DIGITS.
#endif

-- | Format a 'Integer' into decimal ASCII digits.
integer :: Integer -> Builder ()
{-# INLINE integer #-}
integer (IS i#) = int (I# i#)
-- Divide and conquer implementation of string conversion
integer n0
    | n0 < 0    = encodePrim MINUS >> integer' (-n0)
    | otherwise = integer' n0
  where
    integer' :: Integer -> Builder ()
    integer' n
        | n < BASE  = jhead (fromInteger n)
        | otherwise = jprinth (jsplitf (BASE*BASE) n)

    -- Convert a number that has been split into digits in base BASE^2
    -- this includes a last splitting step and then conversion of digits
    -- that all fit into a machine word.
    jprinth :: [Integer] -> Builder ()
    jprinth (n:ns) =
        case n `integerQuotRem#` BASE of
        (# q', r' #) ->
            let q = fromInteger q'
                r = fromInteger r'
            in if q > 0 then jhead q >> jblock r >> jprintb ns
                        else jhead r >> jprintb ns
    jprinth [] = errorWithoutStackTrace "jprinth []"

    jprintb :: [Integer] -> Builder ()
    jprintb []     = pure ()
    jprintb (n:ns) = case n `integerQuotRem#` BASE of
                        (# q', r' #) ->
                            let q = fromInteger q'
                                r = fromInteger r'
                            in jblock q >> jblock r >> jprintb ns

    -- Convert an integer that fits into a machine word. Again, we have two
    -- functions, one that drops leading zeros (jhead) and one that doesn't
    -- (jblock)
    jhead :: Int -> Builder ()
    jhead = int
    jblock :: Int -> Builder ()
    jblock = intWith defaultIFormat{padding = ZeroPadding, width=DIGITS}

    -- Split n into digits in base p. We first split n into digits
    -- in base p*p and then split each of these digits into two.
    -- Note that the first 'digit' modulo p*p may have a leading DIGIT_0
    -- in base p that we need to drop - this is what jsplith takes care of.
    -- jsplitb the handles the remaining digits.
    jsplitf :: Integer -> Integer -> [Integer]
    jsplitf p n
        | p > n     = [n]
        | otherwise = jsplith p (jsplitf (p*p) n)

    jsplith :: Integer -> [Integer] -> [Integer]
    jsplith p (n:ns) =
        case n `integerQuotRem#` p of
        (# q, r #) ->
            if q > 0 then q : r : jsplitb p ns
                     else     r : jsplitb p ns
    jsplith _ [] = errorWithoutStackTrace "jsplith: []"

    jsplitb :: Integer -> [Integer] -> [Integer]
    jsplitb _ []     = []
    jsplitb p (n:ns) = case n `integerQuotRem#` p of
                       (# q, r #) ->
                           q : r : jsplitb p ns

--------------------------------------------------------------------------------

-- | Count how many decimal digits an integer has.
countDigits :: (Integral a) => a -> Int
{-# INLINE countDigits #-}
countDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 19) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 10000000000000000000
        go !k (v :: Word64)
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 1000000000000 =
               k + if v < 100000000
                   then if v < 1000000
                        then if v < 10000
                             then 3
                             else 4 + fin v 100000
                        else 6 + fin v 10000000
                   else if v < 10000000000
                        then 8 + fin v 1000000000
                        else 10 + fin v 100000000000
           | otherwise = go (k + 12) (v `quot` 1000000000000)
        fin v n = if v >= n then 1 else 0

-- | Decimal digit to ASCII digit.
i2wDec :: (Integral a) => a -> Word8
{-# INLINABLE i2wDec #-}
{-# SPECIALIZE INLINE i2wDec :: Int -> Word8 #-}
i2wDec v = DIGIT_0 + fromIntegral v

-- | Hexadecimal digit to ASCII char.
i2wHex :: (Integral a) => a -> Word8
{-# INLINABLE i2wHex #-}
{-# SPECIALIZE INLINE i2wHex :: Int -> Word8 #-}
i2wHex v
    | v <= 9    = DIGIT_0 + fromIntegral v
    | otherwise = 87 + fromIntegral v       -- fromEnum 'a' - 10

-- | Hexadecimal digit to UPPERCASED ASCII char.
i2wHexUpper :: (Integral a) => a -> Word8
{-# INLINABLE i2wHexUpper #-}
{-# SPECIALIZE INLINE i2wHexUpper :: Int -> Word8 #-}
i2wHexUpper v
    | v <= 9    = DIGIT_0 + fromIntegral v
    | otherwise = 55 + fromIntegral v       -- fromEnum 'A' - 10

--------------------------------------------------------------------------------

-- | Format a 'FiniteBits' 'Integral' type into hex nibbles.
--
-- @
-- import Z.Data.Builder as B
-- import Z.Data.Text    as T
-- import Data.Word
-- import Data.Int
--
-- > T.validate . B.build $ B.hex (125 :: Int8)
-- "7d"
-- > T.validate . B.build $ B.hex (-1 :: Int8)
-- "ff"
-- > T.validate . B.build $ B.hex (125 :: Word16)
-- "007d"
-- @
--
hex :: forall a. (FiniteBits a, Integral a) => a -> Builder ()
{-# INLINABLE hex #-}
{-# SPECIALIZE INLINE hex :: Int -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Int8 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Int16 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Int32 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Int64 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word8 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE hex :: Word64 -> Builder () #-}
hex w = writeN hexSiz (go w (hexSiz-2))
  where
    bitSiz = finiteBitSize (undefined :: a)
    hexSiz = (bitSiz+3) `unsafeShiftR` 2
    go !v !d marr off
        | d > 0 = do
            let !i = fromIntegral v .&. 0xFF; !j = i + i
            writePrimArray marr (off + d) $ indexOffPtr hexDigitTable j
            writePrimArray marr (off + d + 1) $ indexOffPtr hexDigitTable (j+1)
            go (v `unsafeShiftR` 8) (d-2) marr off
        | d == 0 = do
            let !i = fromIntegral v .&. 0xFF; !j = i + i
            writePrimArray marr off $ indexOffPtr hexDigitTable j
            writePrimArray marr (off + 1) $ indexOffPtr hexDigitTable (j+1)
        | otherwise = do         -- for FiniteBits instances which has extra bits
            let !i = fromIntegral v .&. 0x0F :: Int
            writePrimArray marr off $ i2wHex i


-- | The UPPERCASED version of 'hex'.
hexUpper :: forall a. (FiniteBits a, Integral a) => a -> Builder ()
{-# INLINABLE hexUpper #-}
{-# SPECIALIZE INLINE hexUpper :: Int -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Int8 -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Int16 -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Int32 -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Int64 -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Word -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Word8 -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Word16 -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Word32 -> Builder () #-}
{-# SPECIALIZE INLINE hexUpper :: Word64 -> Builder () #-}
hexUpper w = writeN hexSiz (go w (hexSiz-2))
  where
    bitSiz = finiteBitSize (undefined :: a)
    hexSiz = (bitSiz+3) `unsafeShiftR` 2
    go !v !d marr off
        | d > 0 = do
            let !i = fromIntegral v .&. 0xFF; !j = i + i
            writePrimArray marr (off + d) $ indexOffPtr hexDigitTableUpper j
            writePrimArray marr (off + d + 1) $ indexOffPtr hexDigitTableUpper (j+1)
            go (v `unsafeShiftR` 8) (d-2) marr off
        | d == 0 = do
            let !i = fromIntegral v .&. 0xFF; !j = i + i
            writePrimArray marr off $ indexOffPtr hexDigitTableUpper j
            writePrimArray marr (off + 1) $ indexOffPtr hexDigitTableUpper (j+1)
        | otherwise = do         -- for FiniteBits instances which has extra bits
            let !i = fromIntegral v .&. 0x0F :: Int
            writePrimArray marr off $ i2wHexUpper i

--------------------------------------------------------------------------------

-- Floating point numbers
-------------------------

-- | Control the rendering of floating point numbers.
data FFormat = Exponent -- ^ Scientific notation (e.g. @2.3e123@).
             | Fixed    -- ^ Standard decimal notation.
             | Generic  -- ^ Use decimal notation for values between @0.1@ and
                        -- @9,999,999@, and scientific notation otherwise.
           deriving (Enum, Read, Show)


-- | Decimal encoding of an IEEE 'Float'.
--
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
float :: Float -> Builder ()
{-# INLINE float #-}
float = floatWith Generic Nothing

-- | Decimal encoding of an IEEE 'Double'.
--
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
double :: Double -> Builder ()
{-# INLINE double #-}
double = doubleWith Generic Nothing

-- | Format single-precision float using drisu3 with dragon4 fallback.
floatWith :: FFormat
          -> Maybe Int  -- ^ Number of decimal places to render.
          -> Float
          -> Builder ()
{-# INLINE floatWith #-}
floatWith fmt decs x
    | isNaN x                   = "NaN"
    | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
    | x < 0                     = char8 '-' >> doFmt fmt decs (grisu3_sp (-x))
    | isNegativeZero x          = char8 '-' >> doFmt fmt decs (0:+IntEnd, 0)
    | x == 0                    = doFmt fmt decs (0:+IntEnd, 0)
    | otherwise                 = doFmt fmt decs (grisu3_sp x) -- Grisu only handles strictly positive finite numbers.

-- | Format double-precision float using drisu3 with dragon4 fallback.
doubleWith :: FFormat
           -> Maybe Int  -- ^ Number of decimal places to render.
           -> Double
           -> Builder ()
{-# INLINE doubleWith #-}
doubleWith fmt decs x
    | isNaN x                   = "NaN"
    | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
    | x < 0                     = char8 '-' >> doFmt fmt decs (grisu3 (-x))
    | isNegativeZero x          = char8 '-' >> doFmt fmt decs (0:+IntEnd, 0)
    | x == 0                    = doFmt fmt decs (0:+IntEnd, 0)
    | otherwise                 = doFmt fmt decs (grisu3 x) -- Grisu only handles strictly positive finite numbers.

-- | A faster version of 'Sci.toDecimalDigits' in case of small coefficient.
positiveSciToDigits :: Sci.Scientific -> (IntList, Int)
{-# INLINE positiveSciToDigits #-}
positiveSciToDigits sci =
    if c == 0
    then (0:+IntEnd, 0)
    else case c of
        (IS i#) -> goI (W# (int2Word# i#)) 0 IntEnd
        _ -> go c 0 IntEnd
  where
    sci' = Sci.normalize sci
    !c = Sci.coefficient sci'
    !e = Sci.base10Exponent sci'

    go :: Integer -> Int -> IntList -> (IntList, Int)
    go 0 !n ds = let !ne = n + e in (ds, ne)
    go i !n ds = case i `integerQuotRem#` 10 of
                     (# q, r #) -> let !d = fromIntegral r in go q (n+1) (d:+ds)
    goI :: Word -> Int -> IntList -> (IntList, Int)
    goI 0 !n ds = let !ne = n + e in (ds, ne)
    goI i !n ds = case quotRem10 i of (q, r) -> let !d = fromIntegral r in goI q (n+1) (d:+ds)

-- | A faster `quotRem` by 10.
quotRem10 :: Word -> (Word, Word)
{-# INLINE quotRem10 #-}
#if SIZEOF_HSWORD < 8
quotRem10 = quotRem 10
#else
quotRem10 (W# w#) =
    let w'# = dquot10# w#
    in (W# w'#, W# (w# `minusWord#` (w'# `timesWord#` 10##)))
  where
    dquot10# :: Word# -> Word#
    dquot10# w =
        let !(# rdx, _ #) = w `timesWord2#` 0xCCCCCCCCCCCCCCCD##
        in rdx `uncheckedShiftRL#` 3#
#endif

-- | Worker function to do formatting.
doFmt :: FFormat
      -> Maybe Int -- ^ Number of decimal places to render.
      -> (IntList, Int) -- ^ List of digits and exponent
      -> Builder ()
{-# INLINABLE doFmt #-}
doFmt format decs (is, e) = case format of
    Generic -> if e < 0 || e > 7 then doFmtExponent else doFmtFixed
    Exponent -> doFmtExponent
    _ -> doFmtFixed
  where
    doFmtExponent = case decs of
        Nothing -> case is of
            (0:+IntEnd) -> "0.0e0"
            (i:+IntEnd) -> encodeDigit i >> ".0e" >> int (e-1)
            (i:+is')    -> do
                encodeDigit i
                encodePrim DOT
                encodeDigits is'
                encodePrim LETTER_e
                int (e-1)
            IntEnd      -> error "doFmt/Exponent: []"
        Just dec
            | dec <= 0 ->
            -- decimal point as well (ghc trac #15115).
            -- Note that this handles negative precisions as well for consistency
            -- (see ghc trac #15509).
                case is of
                    0:+IntEnd -> "0e0"
                    _ -> do
                        let (ei,is') = roundTo10 1 is
                            n:+_ = if ei > 0 then IntList.init is' else is'
                        encodeDigit n
                        encodePrim LETTER_e
                        int (e-1+ei)
        Just dec ->
            let !dec' = max dec 1 in
            case is of
                0:+IntEnd -> "0." >> encodeZeros dec' >> "e0"
                _ -> do
                    let (ei,is') = roundTo10 (dec'+1) is
                        (d:+ds') = if ei > 0 then IntList.init is' else is'
                    encodeDigit d
                    encodePrim DOT
                    encodeDigits ds'
                    encodePrim LETTER_e
                    int (e-1+ei)
    doFmtFixed = case decs of
        Nothing
            | e <= 0    -> do
                "0."
                encodeZeros (-e)
                encodeDigits is
            | otherwise -> insertDot e is
        Just dec ->
            let !dec' = max dec 0
            in if e >= 0
                then do
                    let (ei,is') = roundTo10 (dec' + e) is
                        (ls,rs)  = IntList.splitAt (e+ei) is'
                    mk0 ls
                    (unless (IntList.null rs) $ encodePrim DOT >> encodeDigits rs)
                else do
                    let (ei,is') = roundTo10 dec' (IntList.replicate (-e) 0 IntList.++ is)
                        d:+ds' = if ei > 0 then is' else 0:+is'
                    encodeDigit d
                    (unless (IntList.null ds') $ encodePrim DOT >> encodeDigits ds')

    encodeDigit = word8 . i2wDec

    encodeDigits (d:+ds) =  encodeDigit d >> encodeDigits ds
    encodeDigits _ = pure ()

    encodeZeros n = word8N n DIGIT_0

    mk0 IntEnd = encodePrim DIGIT_0
    mk0 ls     = encodeDigits ls

    insertDot 0      rs = encodePrim DOT >> mk0 rs
    insertDot n  IntEnd = encodePrim DIGIT_0 >> insertDot (n-1) IntEnd
    insertDot n (r:+rs) = encodeDigit r >> insertDot (n-1) rs

roundTo10 :: Int -> IntList -> (Int, IntList)
roundTo10 d is =
    case f d True is of
        x@(0,_) -> x
        (1,xs)  -> (1, 1:+xs)
        _       -> error "roundTo10: impossible"
  where
    b2 = 5
    f n _ IntEnd    = (0, IntList.replicate n 0)
    f 0 e (x:+xs) 
        | x == b2 && e && IntList.all (== 0) xs = (0, IntEnd)   -- Round to even when at exactly half the base
        | otherwise = (if x >= b2 then 1 else 0, IntEnd)
    f n _ (i:+xs)
        | i' == 10 = (1,0:+ds)
        | otherwise  = (0,i':+ds)
      where
        (c,ds) = f (n-1) (even i) xs
        i'     = c + i

------------------------------------------------------------------------------

-- Conversion of 'Float's and 'Double's to ASCII in decimal using Grisu3
------------------------------------------------------------------------

#define GRISU3_SINGLE_BUF_LEN 10
#define GRISU3_DOUBLE_BUF_LEN 18

foreign import ccall unsafe "static grisu3" c_grisu3
    :: Double
    -> MBA# Word8   -- ^ char*
    -> MBA# Int     -- ^ Int
    -> MBA# Int     -- ^ Int
    -> IO Int

-- | Decimal encoding of a 'Double', note grisu only handles strictly positive finite numbers.
grisu3 :: Double -> (IntList, Int)
{-# INLINABLE grisu3 #-}
grisu3 d = unsafePerformIO $ do
    mpa@(MutableByteArray ba#) <- newByteArray GRISU3_DOUBLE_BUF_LEN
    (len, (e, success)) <- allocPrimUnsafe $ \ pLen ->
        allocPrimUnsafe $ \ pE ->
            c_grisu3 (realToFrac d) ba# pLen pE
    if success == 0 -- grisu3 fail
    then do
        let (is, e) = floatToDigits 10 d
        pure (IntList.fromList is, e)
    else do
        is <- go mpa len IntEnd
        pure (is, e+len)
  where 
    go !_   0 acc = return acc
    go !mpa i acc = do
        let i' = i - 1
        w8 <- readByteArray mpa i' :: IO Word8
        go mpa i' (fromIntegral w8 :+ acc)

foreign import ccall unsafe "static grisu3_sp" c_grisu3_sp
    :: Float
    -> MBA# Word8   -- ^ char*
    -> MBA# Int     -- ^ Int
    -> MBA# Int     -- ^ Int
    -> IO Int

-- | Decimal encoding of a 'Float', note grisu3_sp only handles strictly positive finite numbers.
grisu3_sp :: Float -> (IntList, Int)
{-# INLINABLE grisu3_sp #-}
grisu3_sp d = unsafePerformIO $ do
    mpa@(MutableByteArray ba#) <- newByteArray GRISU3_SINGLE_BUF_LEN
    (len, (e, success)) <- allocPrimUnsafe $ \ pLen ->
        allocPrimUnsafe $ \ pE ->
            c_grisu3_sp (realToFrac d) ba# pLen pE
    if success == 0 -- grisu3 fail
    then do
        let (is, e) = floatToDigits 10 d
        pure (IntList.fromList is, e)
    else do
        is <- go mpa len IntEnd
        pure (is, e+len)
  where 
    go !_   0 acc = return acc
    go !mpa i acc = do
        let i' = i - 1
        w8 <- readByteArray mpa i' :: IO Word8
        go mpa i' (fromIntegral w8 :+ acc)

--------------------------------------------------------------------------------

-- | A @Builder@ which renders a scientific number to full
-- precision, using standard decimal notation for arguments whose
-- absolute value lies between @0.1@ and @9,999,999@, and scientific
-- notation otherwise.
scientific :: Sci.Scientific -> Builder ()
{-# INLINE scientific #-}
scientific = scientificWith Generic Nothing

-- | This builder try to avoid scientific notation when 0 <= exponent < 16.
--
scientific' :: Sci.Scientific -> Builder ()
{-# INLINE scientific' #-}
scientific' s
    | e < 0 || e >= 16 = scientific s
    | e == 0 = integer c
    | otherwise = do
        integer c
        when (c /= 0) (replicateM_ e (encodePrim DIGIT_0))
  where
    e = Sci.base10Exponent s
    c = Sci.coefficient s

-- | Like 'scientific' but provides rendering options.
scientificWith :: FFormat
               -> Maybe Int  -- ^ Number of decimal places to render.
               -> Sci.Scientific
               -> Builder ()
{-# INLINE scientificWith #-}
scientificWith fmt decs scntfc
   | scntfc < 0 = char8 '-' <> doFmt fmt decs (positiveSciToDigits (-scntfc))
   | otherwise  =              doFmt fmt decs (positiveSciToDigits   scntfc)
