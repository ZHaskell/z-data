{-|
Module      : Z.Data.JSON.Builder
Description : JSON representation and builders
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides builders for JSON 'Value's, a Haskell JSON representation. These builders are designed to comply with <https://tools.ietf.org/html/rfc8258 rfc8258>. Only control characters are escaped, other unicode codepoints are directly written instead of being escaped.

-}
module Z.Data.JSON.Builder
  ( -- * Value Builders
    value
  , object
  , object'
  , array
  , array'
  , string
  , scientific
    -- * Builder helpers
  , kv, kv'
    -- * Re-export 'Value' type
  , Value(..)
  ) where

import qualified Z.Data.Builder                 as B
import qualified Z.Data.Text                    as T
import qualified Z.Data.Text.ShowT              as T
import           Z.Data.Vector.Base             as V
import           Z.Data.JSON.Value              (Value(..))
import           Data.Scientific              (Scientific, base10Exponent, coefficient)

-- | Use @:@ as separator to connect a label(no need to escape, only add quotes) with field builders.
kv :: T.Text -> B.Builder () -> B.Builder ()
{-# INLINE kv #-}
l `kv` b = B.quotes (B.text l) >> B.colon >> b

-- | Use @:@ as separator to connect a label(escaped and add quotes) with field builders.
kv' :: T.Text -> B.Builder () -> B.Builder ()
{-# INLINE kv' #-}
l `kv'` b = string l >> B.colon >> b

-- | Encode a 'Value', you can use this function with 'toValue' to get 'encodeJSON' with a small overhead.
value :: Value -> B.Builder ()
{-# INLINABLE value #-}
value (Object kvs) = object kvs
value (Array vs) = array vs
value (String t) = string t
value (Number n) = scientific n
value (Bool True) = "true"
value (Bool False) = "false"
value Null = "null"

array :: V.Vector Value -> B.Builder ()
{-# INLINE array #-}
array = B.square . B.intercalateVec B.comma value

array' :: (a -> B.Builder ()) -> V.Vector a -> B.Builder ()
{-# INLINE array' #-}
array' f = B.square . B.intercalateVec B.comma f

object :: V.Vector (T.Text, Value) -> B.Builder ()
{-# INLINE object #-}
object = B.curly . B.intercalateVec B.comma (\ (k, v) -> k `kv'` value v)

object' :: (a -> B.Builder ()) -> V.Vector (T.Text, a) -> B.Builder ()
{-# INLINE object' #-}
object' f = B.curly . B.intercalateVec B.comma (\ (k, v) -> k `kv'` f v)

-- | Escape text into JSON string and add double quotes, escaping rules:
--
-- @
--    \'\\b\':  \"\\b\"
--    \'\\f\':  \"\\f\"
--    \'\\n\':  \"\\n\"
--    \'\\r\':  \"\\r\"
--    \'\\t\':  \"\\t\"
--    \'\"\':  \"\\\"\"
--    \'\\\':  \"\\\\\"
--    \'\/\':  \"\\/\"
--    other chars <= 0x1F: "\\u00XX"
-- @
--
string :: T.Text -> B.Builder ()
{-# INLINE string #-}
string = T.getBuilder . T.escapeTextJSON

-- | This builder try to render integer when (0 <= e < 1024), and scientific notation otherwise.
scientific :: Scientific -> B.Builder ()
{-# INLINE scientific #-}
scientific s
    | e < 0 || e >= 1024 = B.scientific s
    | otherwise = B.integer (coefficient s * 10 ^ e)
  where
    e = base10Exponent s
