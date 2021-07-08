{-|
Module:      Z.Data.Parser.Time
Description : Parsers for types from time.
Copyright:   (c) 2015-2016 Bryan O'Sullivan
             (c) 2020 Dong Han
License:     BSD3
Maintainer:  Dong <winterland1989@gmail.com>
Stability:   experimental
Portability: portable

Parsers for parsing dates and times.
-}

module Z.Data.Parser.Time
    ( day
    , localTime
    , timeOfDay
    , timeZone
    , utcTime
    , zonedTime
    -- * internal
    , fromGregorianValid'
    , fromGregorianValidInt64
    ) where

import           Control.Applicative   ((<|>))
import           Data.Fixed            (Fixed (..), Pico)
import           Data.Int              (Int64)
import           Data.Maybe            (fromMaybe)
import           Data.Time.Calendar    (Day(..), fromGregorianValid)
import           Data.Time.Clock       (UTCTime (..))
import           Data.Time.LocalTime   hiding (utc)
import           Z.Data.ASCII
import qualified Z.Data.Array          as A
import           Z.Data.Parser.Base    (Parser)
import qualified Z.Data.Parser.Base    as P
import qualified Z.Data.Parser.Numeric as P
import qualified Z.Data.Vector         as V
import qualified Z.Data.Text           as T

-- | Parse a date of the form @[+,-]YYYY-MM-DD@.
--
-- Invalid date(leap year rule violation, etc.) will be rejected.
day :: Parser Day
{-# INLINE day #-}
day = "Date must be of form [+,-]YYYY-MM-DD" P.<?> do
    y <- (P.integer <* P.word8 HYPHEN)
    m <- (twoDigits <* P.word8 HYPHEN)
    d <- twoDigits
    case fromGregorianValid' y m d of
        Just d' -> pure d'
        _ -> P.fail' $ T.concat ["Z.Data.Parser.Time.day: invalid date: ", T.toText y, "-", T.toText m, "-", T.toText d]

-- | Faster 'fromGregorianValid' with 'fromGregorianValidInt64' as the common case path.
--
fromGregorianValid' :: Integer -> Int -> Int -> Maybe Day
{-# INLINE fromGregorianValid' #-}
fromGregorianValid' y m d
    | -18000000000000000 < y  && y < 18000000000000000 = fromGregorianValidInt64 (fromIntegral y) m d
    | otherwise = fromGregorianValid y m d

-- | Faster common case for small years(around -18000000000000000 ~ 18000000000000000).
--
fromGregorianValidInt64 :: Int64 -> Int -> Int -> Maybe Day
{-# INLINABLE fromGregorianValidInt64 #-}
fromGregorianValidInt64 year month day_ =
    if (1 <= month && month <= 12) && (1 <= day_ && day_ <= monthLength)
    then Just (ModifiedJulianDay $! fromIntegral mjd)
    else Nothing
  where
    isLeap = (rem year 4 == 0) && ((rem year 400 == 0) || not (rem year 100 == 0))
    dayOfYear =
        let k = if month <= 2 then 0 else if isLeap then -1 else -2
        in ((367 * month - 362) `div` 12) + k + day_
    mjd =
        let y = year - 1
        in (fromIntegral dayOfYear) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678576
    monthLength = A.indexArr (if isLeap then monthListLeap else monthList) (month-1)

monthList :: A.PrimArray Int
{-# NOINLINE monthList #-}
monthList = V.packN 12 [ 31 , 28 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 ]

monthListLeap :: A.PrimArray Int
{-# NOINLINE monthListLeap #-}
monthListLeap = V.packN 12 [ 31 , 29 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 ]

-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
{-# INLINE twoDigits #-}
twoDigits = do
    a <- P.digit
    b <- P.digit
    return $! a * 10 + b

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Parser TimeOfDay
{-# INLINE timeOfDay #-}
timeOfDay = do
    h <- twoDigits
    m <- P.char8 ':' *> twoDigits
    s <- (P.char8 ':' *> seconds) <|> pure 0
    if h < 24 && m < 60 && s < 61
    then return (TimeOfDay h m s)
    else P.fail' "invalid time"

-- | Parse a count of seconds, with the integer part being two digits -- long.
seconds :: Parser Pico
{-# INLINE seconds #-}
seconds = do
    real <- twoDigits
    mw <- P.peekMaybe
    case mw of
        Just DOT -> do
            t <- P.skipWord8 *> P.takeWhile1 isDigit
            return $! parsePicos real t
        _ -> return $! fromIntegral real
 where
    parsePicos a0 t =
        let V.IPair n t'  = V.foldl' step (V.IPair 12 (fromIntegral a0 :: Int64)) t
            step ma@(V.IPair m !a) w
                | m <= 0    = ma
                | otherwise = V.IPair (m-1) (10 * a + P.w2iDec w)
        in MkFixed (fromIntegral (t' * 10^n))

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Parser (Maybe TimeZone)
{-# INLINE timeZone #-}
timeZone = do
    P.skipWhile (== SPACE)
    w <- P.satisfy $ \ w -> w == LETTER_Z || w == PLUS || w == MINUS
    if w == LETTER_Z
    then return Nothing
    else do
        h <- twoDigits
        mm <- P.peekMaybe
        m <- case mm of
               Just COLON         -> P.skipWord8 *> twoDigits
               Just d | isDigit d -> twoDigits
               _                  -> return 0
        let off | w == MINUS = negate off0
                | otherwise  = off0
            off0 = h * 60 + m
        case () of
          _   | off == 0 ->
                return Nothing
              | off < -720 || off > 840 || m > 59 ->
                fail "invalid time zone offset"
              | otherwise ->
                    let !tz = minutesToTimeZone off
                    in return (Just tz)

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@.  The number of seconds is optional
-- and may be followed by a fractional component.
localTime :: Parser LocalTime
{-# INLINE localTime #-}
localTime = LocalTime <$> day <* daySep <*> timeOfDay
  where daySep = P.satisfy (\ w -> w == LETTER_T || w == SPACE)

-- | Behaves as 'zonedTime', but converts any time zone offset into a -- UTC time.
utcTime :: Parser UTCTime
{-# INLINE utcTime #-}
utcTime = do
    lt@(LocalTime d t) <- localTime
    mtz <- timeZone
    case mtz of
        Nothing -> let !tt = timeOfDayToTime t
                   in return (UTCTime d tt)
        Just tz -> return $! localTimeToUTC tz lt

-- | Parse a date with time zone info. Acceptable formats:
--
-- @
--   YYYY-MM-DD HH:MM Z
--   YYYY-MM-DD HH:MM:SS Z
--   YYYY-MM-DD HH:MM:SS.SSS Z
-- @
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
zonedTime :: Parser ZonedTime
{-# INLINE zonedTime #-}
zonedTime = ZonedTime <$> localTime <*> (fromMaybe utc <$> timeZone)

utc :: TimeZone
{-# INLINE utc #-}
utc = TimeZone 0 False ""
