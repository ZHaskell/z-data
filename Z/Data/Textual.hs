{-|
Module      : Z.Data.Text.Print
Description : UTF8 compatible builders.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module re-exports some UTF8 compatible textual builders from 'Z.Data.Encoder'.

We also provide a faster alternative to 'Show' class, i.e. 'Print', which can be deriving using 'Generic'.
For example to use 'Print' class:

@
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}

import qualified Z.Data.Text.Print as T

data Foo = Bar Bytes | Qux Text Int deriving Generic
                                    deriving anyclass T.Print

@

-}

module Z.Data.Text.Print
  ( -- * Print class
    Print(..), toText, toString, toUTF8Encoder, toUTF8Bytes
  -- * Basic UTF8 builders
  , escapeTextJSON
  , B.stringUTF8, B.charUTF8, B.string7, B.char7, B.text
  -- * Numeric builders
  -- ** Integral type formatting
  , B.IFormat(..)
  , B.defaultIFormat
  , B.Padding(..)
  , B.int
  , B.intWith
  , B.integer
  -- ** Fixded size hexidecimal formatting
  , B.hex, B.hexUpper
  -- ** IEEE float formating
  , B.FFormat(..)
  , B.double
  , B.doubleWith
  , B.float
  , B.floatWith
  , B.scientific
  , B.scientific'
  , B.scientificWith
  -- * Helpers
  , B.paren, B.parenWhen, B.curly, B.square, B.angle, B.quotes, B.squotes
  , B.colon, B.comma, B.intercalateVector, B.intercalateList
  ) where

import           Control.Monad
import           Control.Exception              (SomeException)
import           Z.Data.ASCII
import           Data.Fixed
import           Data.Primitive.PrimArray
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.Int
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Monoid                    as Monoid
import           Data.Ratio                     (Ratio, numerator, denominator)
import           Data.Tagged                    (Tagged (..))
import qualified Data.Scientific                as Sci
import qualified Data.Semigroup                 as Semigroup
import           Data.Time                      (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import           Data.Time.Calendar             (CalendarDiffDays (..), DayOfWeek (..))
import           Data.Time.LocalTime            (CalendarDiffTime (..))
import           Data.Time.Clock.System         (SystemTime (..))
import           Data.Typeable
import           Foreign.C.Types
import           GHC.Exts
import           GHC.ForeignPtr
import           GHC.Generics
import           GHC.Natural
import           GHC.Stack
import           GHC.Word
import           GHC.IO
import           Data.Version
import           System.Exit
import           Data.Primitive.Types
import qualified Z.Data.Encoder                 as B
import qualified Z.Data.Text.Base               as T
import           Z.Data.Text.Base               (Text(..))
import qualified Z.Data.Array                   as A
import qualified Z.Data.Vector.Base             as V

#define DOUBLE_QUOTE 34

--------------------------------------------------------------------------------
-- Data types

-- | A class similar to 'Show', serving the purpose that quickly convert a data type to a 'Text' value.
--
-- You can use newtype or generic deriving to implement instance of this class quickly:
--
-- @
--  {-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
--  {-\# LANGUAGE DeriveAnyClass \#-}
--  {-\# LANGUAGE DeriveGeneric \#-}
--  {-\# LANGUAGE DerivingStrategies \#-}
--
--  import GHC.Generics
--
--  newtype FooInt = FooInt Int deriving (Generic)
--                            deriving anyclass Print
--
-- > toText (FooInt 3)
-- > "FooInt 3"
--
--  newtype FooInt = FooInt Int deriving (Generic)
--                            deriving newtype Print
--
-- > toText (FooInt 3)
-- > "3"
-- @
--
class Print a where
    -- | Convert data to 'B.Encoder' with precendence.
    --
    -- You should return a 'B.Encoder' writing in UTF8 encoding only, i.e.
    --
    -- @Z.Data.Text.validateMaybe (Z.Data.Encoder.build (toUTF8EncoderP p a)) /= Nothing@
    toUTF8EncoderP :: Int -> a  -> B.Encoder ()

    default toUTF8EncoderP :: (Generic a, GToText (Rep a)) => Int -> a -> B.Encoder ()
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p = gToUTF8EncoderP p . from

-- | Convert data to 'B.Encoder'.
toUTF8Encoder :: Print a => a  -> B.Encoder ()
{-# INLINABLE toUTF8Encoder #-}
toUTF8Encoder = toUTF8EncoderP 0

-- | Convert data to 'V.Bytes' in UTF8 encoding.
toUTF8Bytes :: Print a => a -> V.Bytes
{-# INLINABLE toUTF8Bytes #-}
toUTF8Bytes = B.build . toUTF8EncoderP 0

-- | Convert data to 'Text'.
toText :: Print a => a -> Text
{-# INLINABLE toText #-}
toText = Text . toUTF8Bytes

-- | Convert data to 'String', faster 'show' replacement.
toString :: Print a => a -> String
{-# INLINABLE toString #-}
toString = T.unpack . toText

class GToText f where
    gToUTF8EncoderP :: Int -> f a -> B.Encoder ()

class GFieldToText f where
    gFieldToUTF8EncoderP :: B.Encoder () -> Int -> f a -> B.Encoder ()

instance (GFieldToText a, GFieldToText b) => GFieldToText (a :*: b) where
    {-# INLINE gFieldToUTF8EncoderP #-}
    gFieldToUTF8EncoderP sep p (a :*: b) =
        gFieldToUTF8EncoderP sep p a >> sep >> gFieldToUTF8EncoderP sep p b

instance (GToText f) => GFieldToText (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFieldToUTF8EncoderP #-}
    gFieldToUTF8EncoderP _ p (M1 x) = gToUTF8EncoderP p x

instance (GToText f, Selector (MetaSel (Just l) u ss ds)) =>
    GFieldToText (S1 (MetaSel (Just l) u ss ds) f) where
        {-# INLINE gFieldToUTF8EncoderP #-}
        gFieldToUTF8EncoderP _ _ m1@(M1 x) =
            B.stringModifiedUTF8 (selName m1) >> " = " >> gToUTF8EncoderP 0 x

instance GToText V1 where
    {-# INLINE gToUTF8EncoderP #-}
    gToUTF8EncoderP _ = error "Z.Data.Text.Print: empty data type"

instance (GToText f, GToText g) => GToText (f :+: g) where
    {-# INLINE gToUTF8EncoderP #-}
    gToUTF8EncoderP p (L1 x) = gToUTF8EncoderP p x
    gToUTF8EncoderP p (R1 x) = gToUTF8EncoderP p x

-- | Constructor without payload, convert to String
instance (Constructor c) => GToText (C1 c U1) where
    {-# INLINE gToUTF8EncoderP #-}
    gToUTF8EncoderP _ m1 = B.stringModifiedUTF8 $ conName m1

-- | Constructor with payloads
instance (GFieldToText (S1 sc f), Constructor c) => GToText (C1 c (S1 sc f)) where
    {-# INLINE gToUTF8EncoderP #-}
    gToUTF8EncoderP p m1@(M1 x) =
        B.parenWhen (p > 10) $ do
            B.stringModifiedUTF8 $ conName m1
            B.char8 ' '
            if conIsRecord m1
            then B.curly $ gFieldToUTF8EncoderP (B.char7 ',' >> B.char7 ' ') p x
            else gFieldToUTF8EncoderP (B.char7 ' ') 11 x

instance (GFieldToText (a :*: b), Constructor c) => GToText (C1 c (a :*: b)) where
    {-# INLINE gToUTF8EncoderP #-}
    gToUTF8EncoderP p m1@(M1 x) =
        case conFixity m1 of
            Prefix -> B.parenWhen (p > 10) $ do
                B.stringModifiedUTF8 $ conName m1
                B.char8 ' '
                if conIsRecord m1
                then B.curly $ gFieldToUTF8EncoderP (B.char7 ',' >> B.char7 ' ') p x
                else gFieldToUTF8EncoderP (B.char7 ' ') 11 x
            Infix _ p' -> B.parenWhen (p > p') $ do
                gFieldToUTF8EncoderP
                    (B.char8 ' ' >> B.stringModifiedUTF8 (conName m1) >> B.char8 ' ') (p'+1) x

instance Print a => GToText (K1 i a) where
    {-# INLINE gToUTF8EncoderP #-}
    gToUTF8EncoderP p (K1 x) = toUTF8EncoderP p x

--------------------------------------------------------------------------------
-- Data types
instance GToText f => GToText (D1 c f) where
    {-# INLINE gToUTF8EncoderP #-}
    gToUTF8EncoderP p (M1 x) = gToUTF8EncoderP p x

instance Print Bool where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ True = "True"
    toUTF8EncoderP _ _    = "False"


instance Print Char where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.string8 . show

instance Print Double where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.double x) ;}
instance Print Float  where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.float x) ;}

instance Print Int     where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.int x) ;}
instance Print Int8    where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.int x) ;}
instance Print Int16   where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.int x) ;}
instance Print Int32   where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.int x) ;}
instance Print Int64   where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.int x) ;}
instance Print Word    where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP _ = B.int;}
instance Print Word8   where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP _ = B.int;}
instance Print Word16  where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP _ = B.int;}
instance Print Word32  where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP _ = B.int;}
instance Print Word64  where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP _ = B.int;}

instance Print Integer  where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.integer x) ;}
instance Print Natural  where {{-# INLINE toUTF8EncoderP #-}; toUTF8EncoderP _ = B.integer . fromIntegral}
instance Print Ordering where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ GT = "GT"
    toUTF8EncoderP _ EQ = "EQ"
    toUTF8EncoderP _ _  = "LT"

instance Print () where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ () = "()"

instance Print Version where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.stringUTF8 . show

-- | The escaping rules is same with 'Show' instance: we reuse JSON escaping rules here, so it will be faster.
instance Print Text where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = escapeTextJSON

-- | Escape text using JSON string escaping rules and add double quotes, escaping rules:
--
-- @
--    \'\\b\':  \"\\b\"
--    \'\\f\':  \"\\f\"
--    \'\\n\':  \"\\n\"
--    \'\\r\':  \"\\r\"
--    \'\\t\':  \"\\t\"
--    \'\"\':  \"\\\"\"
--    \'\\\':  \"\\\\\"
--    other chars <= 0x1F: "\\u00XX"
-- @
--
escapeTextJSON :: T.Text -> B.Encoder ()
{-# INLINABLE escapeTextJSON #-}
escapeTextJSON (T.Text (V.PrimVector ba@(PrimArray ba#) s l)) = do
    let !siz = escape_json_string_length ba# s l
    B.writeN siz (\ mba@(MutablePrimArray mba#) i -> do
        if siz == l+2   -- no need to escape
        then do
            writePrimArray mba i DOUBLE_QUOTE
            copyPrimArray mba (i+1) ba s l
            writePrimArray mba (i+1+l) DOUBLE_QUOTE
        else unsafeIOToST (escape_json_string ba# s l (unsafeCoerce# mba#) i) >> return ())

foreign import ccall unsafe escape_json_string_length
    :: ByteArray# -> Int -> Int -> Int

foreign import ccall unsafe escape_json_string
    :: ByteArray# -> Int -> Int -> MutableByteArray# RealWorld -> Int -> IO Int

instance Print Sci.Scientific where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p x = B.parenWhen (p > 6 && x < 0) (B.scientific x)

instance Print a => Print [a] where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.square . B.intercalateList B.comma (toUTF8EncoderP 0)

instance Print a => Print (A.Array a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.square . B.intercalateVector B.comma (toUTF8EncoderP 0)

instance Print a => Print (A.SmallArray a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.square . B.intercalateVector B.comma (toUTF8EncoderP 0)

instance (A.PrimUnlifted a, Print a) => Print (A.UnliftedArray a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.square . B.intercalateVector B.comma (toUTF8EncoderP 0)

instance (Prim a, Print a) => Print (A.PrimArray a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.square . B.intercalateVector B.comma (toUTF8EncoderP 0)

instance Print a => Print (V.Vector a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.square . B.intercalateVector B.comma (toUTF8EncoderP 0)

instance (Prim a, Print a) => Print (V.PrimVector a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.square . B.intercalateVector B.comma (toUTF8EncoderP 0)

instance (Print a, Print b) => Print (a, b) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ (a, b) = B.paren $  toUTF8EncoderP 0 a
                     >> B.comma >> toUTF8EncoderP 0 b

instance (Print a, Print b, Print c) => Print (a, b, c) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ (a, b, c) = B.paren $  toUTF8EncoderP 0 a
                     >> B.comma >> toUTF8EncoderP 0 b
                     >> B.comma >> toUTF8EncoderP 0 c

instance (Print a, Print b, Print c, Print d) => Print (a, b, c, d) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ (a, b, c, d) = B.paren $  toUTF8EncoderP 0 a
                     >> B.comma >> toUTF8EncoderP 0 b
                     >> B.comma >> toUTF8EncoderP 0 c
                     >> B.comma >> toUTF8EncoderP 0 d

instance (Print a, Print b, Print c, Print d, Print e) => Print (a, b, c, d, e) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ (a, b, c, d, e) = B.paren $  toUTF8EncoderP 0 a
                     >> B.comma >> toUTF8EncoderP 0 b
                     >> B.comma >> toUTF8EncoderP 0 c
                     >> B.comma >> toUTF8EncoderP 0 d
                     >> B.comma >> toUTF8EncoderP 0 e

instance (Print a, Print b, Print c, Print d, Print e, Print f) => Print (a, b, c, d, e, f) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ (a, b, c, d, e, f) = B.paren $  toUTF8EncoderP 0 a
                     >> B.comma >> toUTF8EncoderP 0 b
                     >> B.comma >> toUTF8EncoderP 0 c
                     >> B.comma >> toUTF8EncoderP 0 d
                     >> B.comma >> toUTF8EncoderP 0 e
                     >> B.comma >> toUTF8EncoderP 0 f

instance (Print a, Print b, Print c, Print d, Print e, Print f, Print g) => Print (a, b, c, d, e, f, g) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ (a, b, c, d, e, f, g) = B.paren $  toUTF8EncoderP 0 a
                     >> B.comma >> toUTF8EncoderP 0 b
                     >> B.comma >> toUTF8EncoderP 0 c
                     >> B.comma >> toUTF8EncoderP 0 d
                     >> B.comma >> toUTF8EncoderP 0 e
                     >> B.comma >> toUTF8EncoderP 0 f
                     >> B.comma >> toUTF8EncoderP 0 g

instance Print a => Print (Maybe a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p (Just x) = B.parenWhen (p > 10) $ "Just " >> toUTF8EncoderP 11 x
    toUTF8EncoderP _ _        = "Nothing"

instance (Print a, Print b) => Print (Either a b) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p (Left x) = B.parenWhen (p > 10) $ "Left " >> toUTF8EncoderP 11 x
    toUTF8EncoderP p (Right x) = B.parenWhen (p > 10) $ "Right " >> toUTF8EncoderP 11 x

instance (Print a, Integral a) => Print (Ratio a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p r = B.parenWhen (p > 7) $ do
        toUTF8EncoderP 8 (numerator r)
        " % "
        toUTF8EncoderP 8 (denominator r)

instance HasResolution a => Print (Fixed a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.string8 .  show

instance Print CallStack where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.string8 .  show

deriving newtype instance Print CChar
deriving newtype instance Print CSChar
deriving newtype instance Print CUChar
deriving newtype instance Print CShort
deriving newtype instance Print CUShort
deriving newtype instance Print CInt
deriving newtype instance Print CUInt
deriving newtype instance Print CLong
deriving newtype instance Print CULong
deriving newtype instance Print CPtrdiff
deriving newtype instance Print CSize
deriving newtype instance Print CWchar
deriving newtype instance Print CSigAtomic
deriving newtype instance Print CLLong
deriving newtype instance Print CULLong
deriving newtype instance Print CBool
deriving newtype instance Print CIntPtr
deriving newtype instance Print CUIntPtr
deriving newtype instance Print CIntMax
deriving newtype instance Print CUIntMax
deriving newtype instance Print CClock
deriving newtype instance Print CTime
deriving newtype instance Print CUSeconds
deriving newtype instance Print CSUSeconds
deriving newtype instance Print CFloat
deriving newtype instance Print CDouble

instance Print (Ptr a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ (Ptr a) =
        "0x" >> B.hex (W# (int2Word#(addr2Int# a)))
instance Print (ForeignPtr a) where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ (ForeignPtr a _) =
        "0x" >> B.hex (W# (int2Word#(addr2Int# a)))

deriving anyclass instance Print ExitCode

deriving anyclass instance Print a => Print (Semigroup.Min a)
deriving anyclass instance Print a => Print (Semigroup.Max a)
deriving anyclass instance Print a => Print (Semigroup.First a)
deriving anyclass instance Print a => Print (Semigroup.Last a)
deriving anyclass instance Print a => Print (Semigroup.WrappedMonoid a)
deriving anyclass instance Print a => Print (Semigroup.Dual a)
deriving anyclass instance Print a => Print (Monoid.First a)
deriving anyclass instance Print a => Print (Monoid.Last a)
deriving anyclass instance Print a => Print (NonEmpty a)
deriving anyclass instance Print a => Print (Identity a)
deriving anyclass instance Print a => Print (Const a b)
deriving anyclass instance Print (Proxy a)
deriving anyclass instance Print b => Print (Tagged a b)
deriving anyclass instance Print (f (g a)) => Print (Compose f g a)
deriving anyclass instance (Print (f a), Print (g a)) => Print (Product f g a)
deriving anyclass instance (Print (f a), Print (g a), Print a) => Print (Sum f g a)

--------------------------------------------------------------------------------

-- | @YYYY-MM-DDTHH:MM:SS.SSSZ@
instance Print UTCTime where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.utcTime

-- | @YYYY-MM-DDTHH:MM:SS.SSSZ@
instance Print ZonedTime where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.zonedTime

-- | @YYYY-MM-DD@
instance Print Day where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.day

-- | @YYYY-MM-DDTHH:MM:SS.SSSZ@
instance Print LocalTime where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.localTime

-- | @HH:MM:SS.SSS@
instance Print TimeOfDay where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.timeOfDay

instance Print NominalDiffTime where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.scientific' . realToFrac

instance Print DiffTime where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ = B.scientific' . realToFrac

instance Print SystemTime where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p (MkSystemTime s ns) = B.parenWhen (p > 10) $ do
        "MkSystemTime {systemSeconds = "
        B.int s
        ", systemNanoseconds = "
        B.int ns
        "}"

instance Print CalendarDiffTime where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p (CalendarDiffTime m nt) = B.parenWhen (p > 10) $ do
        B.encodePrim LETTER_P
        B.integer m
        B.encodePrim (LETTER_M, LETTER_T)
        B.scientific' (realToFrac nt)
        B.encodePrim LETTER_S

instance Print CalendarDiffDays where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p (CalendarDiffDays m d) = B.parenWhen (p > 10) $ do
        B.encodePrim LETTER_P
        B.integer m
        B.encodePrim LETTER_M
        B.integer d
        B.encodePrim LETTER_D

instance Print DayOfWeek where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP _ Monday    = "Monday"
    toUTF8EncoderP _ Tuesday   = "Tuesday"
    toUTF8EncoderP _ Wednesday = "Wednesday"
    toUTF8EncoderP _ Thursday  = "Thursday"
    toUTF8EncoderP _ Friday    = "Friday"
    toUTF8EncoderP _ Saturday  = "Saturday"
    toUTF8EncoderP _ Sunday    = "Sunday"

--------------------------------------------------------------------------------

instance Print SomeException where
    {-# INLINE toUTF8EncoderP #-}
    toUTF8EncoderP p x = B.stringUTF8 $ showsPrec p x ""
