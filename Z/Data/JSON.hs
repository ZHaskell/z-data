{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Z.Data.JSON
Description : Fast JSON serialization/deserialization
Copyright   : (c) Dong Han, 2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Types and functions for working efficiently with JSON data, the design is quite similar to @aeson@ or @json@:

  * Encode to bytes can be done directly via 'encodeJSON'.
  * Decode are split in two step, first we parse JSON doc into 'Value', then convert to haskell data via 'fromValue'.
  * 'ToValue' are provided so that other doc formats can be easily supported, such as 'YAML'.

Note this module also provides many (orphan)instances to reduce the compilation stress of a gaint 'Z.Data.JSON.Base' module.

-}
module Z.Data.JSON
  ( -- * How to use this module
    -- $use
    -- ** Custom settings
    -- $custom-settings
    -- ** Write instances manually
    -- $manually-instance

    -- * JSON Class
    JSON(..), Value(..), defaultSettings, Settings(..)
  , snakeCase, trainCase
    -- * Encode & Decode
  , DecodeError
  , decode, decode', decodeText, decodeText'
  , ParseChunks, decodeChunks
  , encode, encodeChunks, encodeText
  , prettyJSON, prettyValue
    -- * parse into JSON Value
  , parseValue, parseValue', parseValueChunks
  -- * Generic functions
  , gToValue, gFromValue, gEncodeJSON
  -- * Convert 'Value' to Haskell data
  , convertValue, Converter(..), fail', (<?>), prependContext
  , PathElement(..), ConvertError(..)
  , typeMismatch, fromNull, withBool, withScientific, withBoundedScientific, withRealFloat
  , withBoundedIntegral, withText, withArray, withKeyValues, withFlatMap, withFlatMapR
  , withHashMap, withHashMapR, withEmbeddedJSON
  , (.:), (.:?), (.:!), convertField, convertFieldMaybe, convertFieldMaybe'
  -- * Helper for manually writing instance.
  , (.=), object, (.!), object', KVItem
  ) where

import           Data.Char
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import qualified Data.Monoid                    as Monoid
import           Data.Proxy                     (Proxy (..))
import           Data.Scientific                (toBoundedInteger)
import qualified Data.Semigroup                 as Semigroup
import           Data.Tagged                    (Tagged (..))
import           Data.Time                      (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import           Data.Time.Calendar             (CalendarDiffDays (..), DayOfWeek (..))
import           Data.Time.LocalTime            (CalendarDiffTime (..))
import           Data.Time.Clock.System         (SystemTime (..))
import           Data.Version                   (Version(versionBranch), makeVersion)
import           Foreign.C.Types
import           System.Exit                    (ExitCode(..))
import qualified Z.Data.Builder                 as B
import           Z.Data.JSON.Base
import qualified Z.Data.JSON.Builder            as JB
import qualified Z.Data.Parser                  as P
import qualified Z.Data.Text                    as T

-- $use
--
-- This module is intended to be used qualified, e.g.
--
-- > import qualified Z.Data.JSON as JSON
-- > import           Z.Data.JSON ((.:), JSON(..))
--
-- The easiest way to use the library is to define target data type, deriving
-- 'GHC.Generics.Generic' and 'JSON' instances, which provides:
--
--   * 'fromValue' to convert 'Value' to Haskell values.
--   * 'toValue' to convert Haskell values to 'Value'.
--   * 'encodeJSON' to directly write Haskell value into JSON bytes.
--
-- For example,
--
-- > {-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- >
-- > import GHC.Generics (Generic)
-- > import qualified Z.Data.Builder as Builder
-- > import qualified Z.Data.JSON as JSON
-- > import qualified Z.Data.Text as T
-- >
-- > data Person = Person {name :: T.Text, age :: Int}
-- >     deriving (Show, Generic)
-- >     deriving anyclass (JSON.JSON)
--
-- We can now encode & decode with 'T.Text' like so:
--
-- >>> JSON.encodeText (Person{ name="Alice", age=16 })
-- "{\"age\":16,\"name\":\"Alice\"}"
-- >>> JSON.decodeText' "{\"age\":16,\"name\":\"Alice\"}" :: Either JSON.DecodeError Person
-- Right (Person {age = 16, name = "Alice"})
--
-- The 'GHC.Generics.Generic' instances convert(encode) Haskell data with following rules:
--
--   * Constructors without payloads are encoded as JSON String, @data T = A | B@ are encoded as @\"A\"@ or @\"B\"@.
--   * Single constructor are ingored if there're payloads, @data T = T ...@,  @T@ is ingored:
--
--     * Records are encoded as JSON object. @data T = T{k1 :: .., k2 :: ..}@ are encoded as @{\"k1\":...,\"k2\":...}@.
--     * Plain product are encoded as JSON array. @data T = T t1 t2@ are encoded as "[x1,x2]".
--     * Single field plain product are encoded as it is, i.e. @data T = T t@ are encoded as \"x\" just like its payload.
--
--   * Multiple constructors are convert to single key JSON object if there're payloads:
--
--     * Records are encoded as JSON object like above. @data T = A | B {k1 :: .., k2 :: ..}@ are encoded as
--         @{\"B\":{\"k1\":...,\"k2\":...}}@ in @B .. ..@ case, or @\"A\"@ in @A@ case.
--     * Plain product are similar to above, wrappered by an outer single-key object layer marking which constructor.
--
-- These rules apply to user defined ADTs, but some built-in instances have
-- different behaviour, namely:
--
--   * @Maybe a@ are encoded as JSON @null@ in 'Nothing' case, or directly encoded to its payload in 'Just' case.
--   * @[a]@ are encoded to JSON array, @[Char]@ are encoded into JSON string.
--   * 'NonEmpty', 'Vector', 'PrimVector', 'HashSet', 'FlatSet', 'FlatIntSet' are also encoded to JSON array.
--   * 'Bytes' are encoded into JSON text using base64 encoding.
--   * 'HashMap', 'FlatMap', 'FlatIntMap' are encoded to JSON object.

-- $custom-settings
--
-- There're some modifying options if you providing a custom 'Settings', which
-- allow you to modify field name or constructor name, but please /DO NOT/
-- produce control characters during your modification, since we assume field
-- labels and constructor name won't contain them, thus we can save an extra
-- escaping pass. To use custom 'Settings' just write:
--
-- > data T = T {fooT :: Int, barT :: [Int]} deriving Generic
-- > instance JSON.ToValue T where
-- >     -- You can omit following definition if you don't need to change settings
-- >     toValue = JSON.gToValue JSON.defaultSettings{ JSON.fieldFmt = JSON.snakeCase } . from
-- >
-- > -- define this instances if you need fast JSON encoding(without convert to JSON.Value first)
-- > instance JSON.EncodeJSON T where
-- >     -- You can omit following definition if you don't need to change settings
-- >     encodeJSON = JSON.gEncodeJSON JSON.defaultSettings{ JSON.fieldFmt = JSON.snakeCase } . from
--
-- >>> JSON.toValue (T 0 [1,2,3])
-- Object [("foo_t",Number 0.0),("bar_t",Array [Number 1.0,Number 2.0,Number 3.0])]
--
-- $manually-instance
--
-- You can write 'JSON' instances by hand if the 'Generic' based one doesn't suit you.
-- Here is an example similar to aeson's.
--
-- @
-- import qualified Z.Data.Text          as T
-- import qualified Z.Data.Vector        as V
-- import qualified Z.Data.Builder       as B
-- import qualified Z.Data.JSON          as JSON
-- import           Z.Data.JSON          ((.:), (.=), (.!), JSON(..))
--
-- data Person = Person { name :: T.Text , age  :: Int } deriving Show
--
-- instance JSON Person where
--     fromValue = JSON.withFlatMapR \"Person\" $ \\ v -> Person
--                     \<$\> v .: \"name\"
--                     \<*\> v .: \"age\"
--
--     toValue (Person n a) = JSON.object [\"name\" .= n, \"age\" .= a]
--
--     encodeJSON (Person n a) = JSON.object' $ (\"name\" .! n <> \"age\" .! a)
-- @
--
-- >>> toValue (Person "Joe" 12)
-- Object [("name",String "Joe"),("age",Number 12.0)]
-- >>> JSON.convert' @Person . JSON.Object $ V.pack [("name",JSON.String "Joe"),("age",JSON.Number 12.0)]
-- Right (Person {name = "Joe", age = 12})
-- >>> JSON.encodeText (Person "Joe" 12)
-- "{"name":"Joe","age":12}"
--
-- The 'Value' type is different from aeson's one in that we use @Vector (Text, Value)@ to represent JSON objects, thus
-- we can choose different strategies on key duplication, the lookup map type, etc. so instead of a single 'withObject',
-- we provide 'withHashMap', 'withHashMapR', 'withFlatMap' and 'withFlatMapR' which use different lookup map type, and different key order piority. Most of time 'FlatMap' is faster than 'HashMap' since we only use the lookup map once, the cost of constructing a 'HashMap' is higher. If you want to directly working on key-values, 'withKeyValues' provide key-values vector access.
--
-- There're some useful tools to help write encoding code in "Z.Data.JSON.Builder" module, such as JSON string escaping tool, etc.
--
-- If you don't particularly care for fast encoding, you can also use 'toValue' together with value builder, the overhead is usually very small.


-- | Snake casing a pascal cased constructor name or camel cased field name, words are always lower cased and separated by an
-- underscore.
snakeCase :: String -> T.Text
{-# INLINE snakeCase #-}
snakeCase = symbCase '_'

-- | Train casing a pascal cased constructor name or camel cased field name, words are always lower cased and separated by
-- a hyphen.
trainCase :: String -> T.Text
{-# INLINE trainCase #-}
trainCase = symbCase '-'

--------------------------------------------------------------------------------

symbCase :: Char -> String -> T.Text
{-# INLINE symbCase #-}
symbCase sym =  T.pack . go . applyFirst toLower
  where
    go []                       = []
    go (x:xs) | isUpper x = sym : toLower x : go xs
              | otherwise = x : go xs

    applyFirst _ []     = []
    applyFirst f (x:xs) = f x: xs

--------------------------------------------------------------------------------

instance JSON ExitCode where
    {-# INLINE fromValue #-}
    fromValue (String "ExitSuccess") = return ExitSuccess
    fromValue (Number x) =
        case toBoundedInteger x of
            Just i -> return (ExitFailure i)
            _      -> fail' . B.unsafeBuildText $ do
                "converting ExitCode failed, value is either floating or will cause over or underflow: "
                B.scientific x
    fromValue _ =  fail' "converting ExitCode failed, expected a string or number"

    {-# INLINE toValue #-}
    toValue ExitSuccess     = String "ExitSuccess"
    toValue (ExitFailure n) = Number (fromIntegral n)

    {-# INLINE encodeJSON #-}
    encodeJSON ExitSuccess     = "\"ExitSuccess\""
    encodeJSON (ExitFailure n) = B.int n

-- | Only round trip 'versionBranch' as JSON array.
instance JSON Version where
    {-# INLINE fromValue #-}
    fromValue v = makeVersion <$> fromValue v
    {-# INLINE toValue #-}
    toValue = toValue . versionBranch
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . versionBranch

--------------------------------------------------------------------------------

-- | @YYYY-MM-DDTHH:MM:SS.SSSZ@
instance JSON UTCTime where
    {-# INLINE fromValue #-}
    fromValue = withText "UTCTime" $ \ t ->
        case P.parse' (P.utcTime <* P.endOfInput) (T.getUTF8Bytes t) of
            Left err -> fail' $ "could not parse date as UTCTime: " <> T.toText err
            Right r  -> return r
    {-# INLINE toValue #-}
    toValue t = String (B.unsafeBuildText (B.utcTime t))
    {-# INLINE encodeJSON #-}
    encodeJSON = B.quotes . B.utcTime

-- | @YYYY-MM-DDTHH:MM:SS.SSSZ@
instance JSON ZonedTime where
    {-# INLINE fromValue #-}
    fromValue = withText "ZonedTime" $ \ t ->
        case P.parse' (P.zonedTime <* P.endOfInput) (T.getUTF8Bytes t) of
            Left err -> fail' $ "could not parse date as ZonedTime: " <> T.toText err
            Right r  -> return r
    {-# INLINE toValue #-}
    toValue t = String (B.unsafeBuildText (B.zonedTime t))
    {-# INLINE encodeJSON #-}
    encodeJSON = B.quotes . B.zonedTime

-- | @YYYY-MM-DD@
instance JSON Day where
    {-# INLINE fromValue #-}
    fromValue = withText "Day" $ \ t ->
        case P.parse' (P.day <* P.endOfInput) (T.getUTF8Bytes t) of
            Left err -> fail' $ "could not parse date as Day: " <> T.toText err
            Right r  -> return r
    {-# INLINE toValue #-}
    toValue t = String (B.unsafeBuildText (B.day t))
    {-# INLINE encodeJSON #-}
    encodeJSON = B.quotes . B.day


-- | @YYYY-MM-DDTHH:MM:SS.SSSZ@
instance JSON LocalTime where
    {-# INLINE fromValue #-}
    fromValue = withText "LocalTime" $ \ t ->
        case P.parse' (P.localTime <* P.endOfInput) (T.getUTF8Bytes t) of
            Left err -> fail' $ "could not parse date as LocalTime: " <> T.toText err
            Right r  -> return r
    {-# INLINE toValue #-}
    toValue t = String (B.unsafeBuildText (B.localTime t))
    {-# INLINE encodeJSON #-}
    encodeJSON = B.quotes . B.localTime

-- | @HH:MM:SS.SSS@
instance JSON TimeOfDay where
    {-# INLINE fromValue #-}
    fromValue = withText "TimeOfDay" $ \ t ->
        case P.parse' (P.timeOfDay <* P.endOfInput) (T.getUTF8Bytes t) of
            Left err -> fail' $ "could not parse time as TimeOfDay: " <> T.toText err
            Right r  -> return r
    {-# INLINE toValue #-}
    toValue t = String (B.unsafeBuildText (B.timeOfDay t))
    {-# INLINE encodeJSON #-}
    encodeJSON = B.quotes . B.timeOfDay

-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'NominalDiffTime' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance JSON NominalDiffTime where
    {-# INLINE fromValue #-}
    fromValue = withBoundedScientific "NominalDiffTime" $ pure . realToFrac
    {-# INLINE toValue #-}
    toValue = Number . realToFrac
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.scientific . realToFrac

-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'DiffTime' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance JSON DiffTime where
    {-# INLINE fromValue #-}
    fromValue = withBoundedScientific "DiffTime" $ pure . realToFrac
    {-# INLINE toValue #-}
    toValue = Number . realToFrac
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.scientific . realToFrac

-- | @{"seconds": SSS, "nanoseconds": NNN}@.
instance JSON SystemTime where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "SystemTime" $ \ v ->
        MkSystemTime <$> v .: "seconds" <*> v .: "nanoseconds"
    {-# INLINE toValue #-}
    toValue (MkSystemTime s ns) = object [ "seconds" .= s , "nanoseconds" .= ns ]
    {-# INLINE encodeJSON #-}
    encodeJSON (MkSystemTime s ns) = object' ("seconds" .! s <> "nanoseconds" .! ns)

instance JSON CalendarDiffTime where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "CalendarDiffTime" $ \ v ->
        CalendarDiffTime <$> v .: "months" <*> v .: "time"
    {-# INLINE toValue #-}
    toValue (CalendarDiffTime m nt) = object [ "months" .= m , "time" .= nt ]
    {-# INLINE encodeJSON #-}
    encodeJSON (CalendarDiffTime m nt) = object' ("months" .! m <> "time" .! nt)

instance JSON CalendarDiffDays where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "CalendarDiffDays" $ \ v ->
        CalendarDiffDays <$> v .: "months" <*> v .: "days"
    {-# INLINE toValue #-}
    toValue (CalendarDiffDays m d) = object ["months" .= m, "days" .= d]
    {-# INLINE encodeJSON #-}
    encodeJSON (CalendarDiffDays m d) = object' ("months" .! m <> "days" .! d)

instance JSON DayOfWeek where
    {-# INLINE fromValue #-}
    fromValue (String "Monday"   ) = pure Monday
    fromValue (String "Tuesday"  ) = pure Tuesday
    fromValue (String "Wednesday") = pure Wednesday
    fromValue (String "Thursday" ) = pure Thursday
    fromValue (String "Friday"   ) = pure Friday
    fromValue (String "Saturday" ) = pure Saturday
    fromValue (String "Sunday"   ) = pure Sunday
    fromValue (String _   )        = fail' "converting DayOfWeek failed, value should be one of weekdays"
    fromValue v                    = typeMismatch "DayOfWeek" "String" v

    {-# INLINE toValue #-}
    toValue Monday    = String "Monday"
    toValue Tuesday   = String "Tuesday"
    toValue Wednesday = String "Wednesday"
    toValue Thursday  = String "Thursday"
    toValue Friday    = String "Friday"
    toValue Saturday  = String "Saturday"
    toValue Sunday    = String "Sunday"

    {-# INLINE encodeJSON #-}
    encodeJSON Monday    = "\"Monday\""
    encodeJSON Tuesday   = "\"Tuesday\""
    encodeJSON Wednesday = "\"Wednesday\""
    encodeJSON Thursday  = "\"Thursday\""
    encodeJSON Friday    = "\"Friday\""
    encodeJSON Saturday  = "\"Saturday\""
    encodeJSON Sunday    = "\"Sunday\""


--------------------------------------------------------------------------------

deriving newtype instance JSON (f (g a)) => JSON (Compose f g a)
deriving newtype instance JSON a => JSON (Semigroup.Min a)
deriving newtype instance JSON a => JSON (Semigroup.Max a)
deriving newtype instance JSON a => JSON (Semigroup.First a)
deriving newtype instance JSON a => JSON (Semigroup.Last a)
deriving newtype instance JSON a => JSON (Semigroup.WrappedMonoid a)
deriving newtype instance JSON a => JSON (Semigroup.Dual a)
deriving newtype instance JSON a => JSON (Monoid.First a)
deriving newtype instance JSON a => JSON (Monoid.Last a)
deriving newtype instance JSON a => JSON (Identity a)
deriving newtype instance JSON a => JSON (Const a b)
deriving newtype instance JSON b => JSON (Tagged a b)

-- | Use 'Null' as @Proxy a@
instance JSON (Proxy a) where
    {-# INLINE fromValue #-}; fromValue = fromNull "Proxy" Proxy;
    {-# INLINE toValue #-}; toValue _ = Null;
    {-# INLINE encodeJSON #-}; encodeJSON _ = "null";

--------------------------------------------------------------------------------

deriving newtype instance JSON CChar
deriving newtype instance JSON CSChar
deriving newtype instance JSON CUChar
deriving newtype instance JSON CShort
deriving newtype instance JSON CUShort
deriving newtype instance JSON CInt
deriving newtype instance JSON CUInt
deriving newtype instance JSON CLong
deriving newtype instance JSON CULong
deriving newtype instance JSON CPtrdiff
deriving newtype instance JSON CSize
deriving newtype instance JSON CWchar
deriving newtype instance JSON CSigAtomic
deriving newtype instance JSON CLLong
deriving newtype instance JSON CULLong
deriving newtype instance JSON CBool
deriving newtype instance JSON CIntPtr
deriving newtype instance JSON CUIntPtr
deriving newtype instance JSON CIntMax
deriving newtype instance JSON CUIntMax
deriving newtype instance JSON CClock
deriving newtype instance JSON CTime
deriving newtype instance JSON CUSeconds
deriving newtype instance JSON CSUSeconds
deriving newtype instance JSON CFloat
deriving newtype instance JSON CDouble

--------------------------------------------------------------------------------

deriving anyclass instance (JSON (f a), JSON (g a), JSON a) => JSON (Sum f g a)
deriving anyclass instance (JSON a, JSON b) => JSON (Either a b)
deriving anyclass instance (JSON (f a), JSON (g a)) => JSON (Product f g a)

deriving anyclass instance (JSON a, JSON b) => JSON (a, b)
deriving anyclass instance (JSON a, JSON b, JSON c) => JSON (a, b, c)
deriving anyclass instance (JSON a, JSON b, JSON c, JSON d) => JSON (a, b, c, d)
deriving anyclass instance (JSON a, JSON b, JSON c, JSON d, JSON e) => JSON (a, b, c, d, e)
deriving anyclass instance (JSON a, JSON b, JSON c, JSON d, JSON e, JSON f) => JSON (a, b, c, d, e, f)
deriving anyclass instance (JSON a, JSON b, JSON c, JSON d, JSON e, JSON f, JSON g) => JSON (a, b, c, d, e, f, g)
