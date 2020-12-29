{-|
Module:      Z.Data.Parser.Time
Description : Builders for types from time.
Copyright:   (c) 2015-2016 Bryan O'Sullivan
             (c) 2020 Dong Han
License:     BSD3
Maintainer:  Dong <winterland1989@gmail.com>
Stability:   experimental
Portability: portable

Builders for dates and times.
-}

module Z.Data.Builder.Time
  ( day
  , timeOfDay
  , timeZone
  , utcTime
  , localTime
  , zonedTime
  ) where

import Control.Monad
import Data.Time
import Data.Word
import Data.Fixed
import Data.Int
import           Z.Data.Builder.Base        (Builder)
import qualified Z.Data.Builder.Base        as B
import qualified Z.Data.Builder.Numeric     as B
import Z.Data.Builder.Numeric   (i2wDec)
import Z.Data.ASCII

-- | @YYYY-mm-dd@.
day :: Day -> Builder ()
{-# INLINE day #-}
day dd = encodeYear yr <>
         B.encodePrim (HYPHEN, mh, ml, HYPHEN, dh, dl)
  where (yr, m, d)    = toGregorian dd
        (mh, ml)  = twoDigits m
        (dh, dl)  = twoDigits d
        encodeYear y
            | y >= 1000 = B.integer y
            | y >= 0    = B.encodePrim (padYear y)
            | y >= -999 = B.encodePrim (MINUS, padYear y)
            | otherwise = B.integer y
        padYear y =
            let (ab,c) = (fromIntegral y :: Int) `quotRem` 10
                (a, b)  = ab `quotRem` 10
            in (DIGIT_0, i2wDec a, i2wDec b, i2wDec c)

-- | @HH-MM-SS@.
timeOfDay :: TimeOfDay -> Builder ()
{-# INLINE timeOfDay #-}
timeOfDay t = timeOfDay64 (toTimeOfDay64 t)

-- | Timezone format in @+HH:MM@, with single letter @Z@ for @+00:00@.
timeZone :: TimeZone -> Builder ()
{-# INLINE timeZone #-}
timeZone (TimeZone off _ _)
  | off == 0  = B.word8 LETTER_Z
  | otherwise = B.encodePrim (s, hh, hl, COLON, mh, ml)
  where !s         = if off < 0 then MINUS else PLUS
        (hh, hl)   = twoDigits h
        (mh, ml)   = twoDigits m
        (h,m)      = abs off `quotRem` 60

-- | Write 'UTCTime' in ISO8061 @YYYY-MM-DDTHH:MM:SS.SSSZ@(time zone will always be @Z@).
utcTime :: UTCTime -> Builder ()
{-# INLINE utcTime #-}
utcTime (UTCTime d s) = dayTime d (diffTimeOfDay64 s) >> B.word8 LETTER_Z

-- | Write 'LocalTime' in ISO8061 @YYYY-MM-DDTHH:MM:SS.SSS@.
localTime :: LocalTime -> Builder ()
{-# INLINE localTime #-}
localTime (LocalTime d t) = dayTime d (toTimeOfDay64 t)

-- | Write 'ZonedTime' in ISO8061 @YYYY-MM-DD HH:MM:SS.SSSZ@.
zonedTime :: ZonedTime -> Builder ()
{-# INLINE zonedTime #-}
zonedTime (ZonedTime t z) = localTime t >> timeZone z

--------------------------------------------------------------------------------

-- | Like TimeOfDay, but using a fixed-width integer for seconds.
type TimeOfDay64 = (Int, Int, Int64)

diffTimeOfDay64 :: DiffTime -> TimeOfDay64
{-# INLINE diffTimeOfDay64 #-}
diffTimeOfDay64 t
  | t >= 86400 = (23, 59, 60000000000000 + pico (t - 86400))
  | otherwise = (fromIntegral h, fromIntegral m, s)
    where (h,mp) = pico t `quotRem` 3600000000000000
          (m,s)  = mp `quotRem` 60000000000000
          pico   = fromIntegral . diffTimeToPicoseconds

toTimeOfDay64 :: TimeOfDay -> TimeOfDay64
{-# INLINE toTimeOfDay64 #-}
toTimeOfDay64 (TimeOfDay h m (MkFixed s)) = (h, m, fromIntegral s)

dayTime :: Day -> TimeOfDay64 -> Builder ()
{-# INLINE dayTime #-}
dayTime d t = day d >> B.word8 LETTER_T >> timeOfDay64 t

timeOfDay64 :: TimeOfDay64 -> Builder ()
{-# INLINE timeOfDay64 #-}
timeOfDay64 (!h, !m, !s) = do
    B.encodePrim (hh, hl, COLON, mh, ml, COLON, sh, sl)
    when (frac /= 0) $ do
        B.encodePrim DOT
        replicateM_ (12 - n1) (B.word8 DIGIT_0)
        forM_ ds (B.word8 . B.i2wDec)
  where
    (real, frac) = s `quotRem` 1000000000000 -- number of picoseconds  in 1 second

    (hh, hl)     = twoDigits h
    (mh, ml)     = twoDigits m
    (sh, sl)     = twoDigits (fromIntegral real)

    (frac', n0) = ctz frac 0
    (ds, n1) = toDigits frac' [] n0

    ctz :: Int64 -> Int -> (Int64, Int)
    ctz !x !n =
        let (x', r) = x `quotRem` 10
        in if r == 0
            then ctz x' (n+1)
            else (x, n)

    toDigits :: Int64 -> [Int64] -> Int -> ([Int64], Int)
    toDigits !x !acc !n =
        let (x', r) = x `quotRem` 10
        in if x' == 0
            then ((r:acc), n+1)
            else toDigits x' (r:acc) (n+1)

twoDigits :: Int -> (Word8, Word8)
{-# INLINE twoDigits #-}
twoDigits a = (i2wDec hi, i2wDec lo)
  where (hi,lo) = a `quotRem` 10

