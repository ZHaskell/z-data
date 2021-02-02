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
  , decode, decode', decodeText, decodeText', ParseChunks, decodeChunks
  , encode, encodeChunks, encodeText
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
import           Z.Data.JSON.Base
import qualified Z.Data.Text      as T

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
