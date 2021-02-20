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
  , prettyValue
  , prettyValue'
    -- * Builder helpers
  , kv, kv'
    -- * Re-export 'Value' type
  , Value(..)
  ) where

import           Control.Monad
import           Z.Data.ASCII
import qualified Z.Data.Builder                 as B
import qualified Z.Data.Text                    as T
import qualified Z.Data.Text.Print              as T
import           Z.Data.Vector.Base             as V
import           Z.Data.JSON.Value              (Value(..))
import           Data.Scientific                (Scientific, base10Exponent, coefficient)

-- | Use @:@ as separator to connect a label(no escape, only add quotes) with field builders.
--
-- Don't use chars which need escaped in label.
kv :: T.Text -> B.Builder () -> B.Builder ()
{-# INLINE kv #-}
l `kv` b = B.quotes (B.text l) >> B.colon >> b

-- | Use @:@ as separator to connect a label(escape the label and add quotes) with field builders.
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
value _ = "null"

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
--    \'\DEL\':  \"\\u007f\"
--    other chars <= 0x1F: "\\u00XX"
-- @
--
string :: T.Text -> B.Builder ()
{-# INLINE string #-}
string = T.escapeTextJSON

-- | This builder try to render integer when (0 <= e < 16), and scientific notation otherwise.
scientific :: Scientific -> B.Builder ()
{-# INLINE scientific #-}
scientific s
    | e < 0 || e >= 16 = B.scientific s
    | e == 0 = B.integer c
    | otherwise = do
        B.integer c
        when (c /= 0) (replicateM_ e (B.encodePrim DIGIT_0))
  where
    e = base10Exponent s
    c = coefficient s

--------------------------------------------------------------------------------

-- | 'ValuePretty\'' with 4 spaces indentation per level, e.g.
--
-- @
-- {
--     "results":
--     [
--         {
--             "from_user_id_str":"80430860",
--             "profile_image_url":"http://a2.twimg.com/profile_images/536455139/icon32_normal.png",
--             "created_at":"Wed, 26 Jan 2011 07:07:02 +0000",
--             "from_user":"kazu_yamamoto",
--             "id_str":"30159761706061824",
--             "metadata":
--             {
--                 "result_type":"recent"
--             },
--             "to_user_id":null,
--             "text":"Haskell Server Pages って、まだ続いていたのか！",
--             "id":30159761706061824,
--             "from_user_id":80430860,
--             "geo":null,
--             "iso_language_code":"no",
--             "to_user_id_str":null,
--             "source":"&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;"
--         }
--     ],
--     "max_id":30159761706061824,
--     "since_id":0,
--     "refresh_url":"?since_id=30159761706061824&q=haskell",
--     "next_page":"?page=2&max_id=30159761706061824&rpp=1&q=haskell",
--     "results_per_page":1,
--     "page":1,
--     "completed_in":1.2606e-2,
--     "since_id_str":"0",
--     "max_id_str":"30159761706061824",
--     "query":"haskell"
-- }
-- @
--
prettyValue :: Value -> B.Builder ()
prettyValue = prettyValue' 4 0


-- | Encode a 'Value' with indentation and linefeed.
prettyValue' :: Int  -- ^ indentation per level
             -> Int  -- ^ initial indentation
             -> Value -> B.Builder ()
{-# INLINABLE prettyValue' #-}
prettyValue' c !ind (Object kvs) = objectPretty c ind kvs
prettyValue' c !ind (Array vs)   = arrayPretty c ind vs
prettyValue' _ !ind (String t)   = B.word8N ind SPACE >> string t
prettyValue' _ !ind (Number n)   = B.word8N ind SPACE >> scientific n
prettyValue' _ !ind (Bool True)  = B.word8N ind SPACE >> "true"
prettyValue' _ !ind (Bool False) = B.word8N ind SPACE >> "false"
prettyValue' _ !ind _            = B.word8N ind SPACE >> "null"

arrayPretty :: Int -> Int -> V.Vector Value -> B.Builder ()
{-# INLINE arrayPretty #-}
arrayPretty idpl ind vs
    | V.null vs = B.word8N ind SPACE >> B.square (return ())
    | otherwise = do
        B.word8N ind SPACE
        B.encodePrim (SQUARE_LEFT, NEWLINE)
        B.intercalateVec
            (B.encodePrim (COMMA, NEWLINE))
            (prettyValue' idpl ind')
            vs
        B.word8 NEWLINE
        B.word8N ind SPACE
        B.word8 SQUARE_RIGHT
  where
    ind' = ind + idpl

objectPretty :: Int -> Int -> V.Vector (T.Text, Value) -> B.Builder ()
{-# INLINE objectPretty #-}
objectPretty idpl ind kvs
    | V.null kvs = B.word8N ind SPACE >> B.curly (return ())
    | otherwise = do
        B.word8N ind SPACE
        B.encodePrim (CURLY_LEFT, NEWLINE)
        B.intercalateVec
            (B.encodePrim (COMMA, NEWLINE))
            (\ (k, v) -> do
                B.word8N ind' SPACE
                string k
                B.colon
                if isSimpleValue v
                then prettyValue' idpl 0 v
                else do
                    B.word8 NEWLINE
                    prettyValue' idpl ind' v)
            kvs
        B.word8 NEWLINE
        B.word8N ind SPACE
        B.word8 CURLY_RIGHT
  where
    ind' = ind + idpl
    isSimpleValue v = case v of
        (Object kvs') -> V.null kvs'
        (Array vs) -> V.null vs
        _ -> True
