{-|
Module      : Z.Data.JSON.Value
Description : JSON representation and parsers
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides definition and parsers for JSON 'Value's, a Haskell JSON representation. The parsers is designed to comply with <https://tools.ietf.org/html/rfc8258 rfc8258>, notable pitfalls are:

  * The numeric representation use 'Scientific', which impose a limit on number's exponent part(limited to 'Int').
  * Unescaped control characters(<=0x1F) are NOT accepted, (different from aeson).
  * Only @0x20, 0x09, 0x0A, 0x0D@ are valid JSON whitespaces, 'skipSpaces' from this module is different from 'P.skipSpaces'.
  * A JSON document shouldn't have trailing characters except whitespaces describe above, see 'parseValue''
    and 'parseValueChunks''.
  * Objects are represented as key-value vectors, key order and duplicated keys are preserved for further processing.

Note that rfc8258 doesn't enforce unique key in objects, it's up to users to decided how to deal with key duplication, e.g. prefer first or last key, see 'Z.Data.JSON.Base.withFlatMap' or 'Std.Data.JSON.Base.withFlatMapR' for example.

There's no lazy parsers here, every pieces of JSON document will be parsed into a normal form 'Value'. 'Object' and 'Array's payloads are packed into 'Vector's to avoid accumulating lists in memory. Read more about <http://winterland.me/2019/03/05/aeson's-mysterious-lazy-parsing why no lazy parsing is needed>.
-}

module Z.Data.JSON.Value
  ( -- * Value type
    Value(..), key, nth
    -- * parse into JSON Value
  , parseValue
  , parseValue'
    -- * Value Parsers
  , value
  , object
  , array
  , string
  , skipSpaces
    -- * Convert to Scientific
  , floatToScientific
  , doubleToScientific
  ) where

import           Control.DeepSeq
import           Data.Bits                  ((.&.))
import           Data.Functor
import           Data.Scientific            (Scientific, scientific)
import           Data.Typeable
import           Data.Int
import           Data.Word
import           GHC.Generics
import           Z.Data.ASCII
import qualified Z.Data.Parser              as P
import qualified Z.Data.Builder.Numeric     as B
import qualified Z.Data.Text.Base           as T
import           Z.Data.Text.Print          (Print(..))
import           Z.Data.Vector.Base         as V
import           Z.Data.Vector.Extra        as V
import           Z.Data.Vector.Search       as V
import           Z.Foreign
import           System.IO.Unsafe           (unsafeDupablePerformIO)
import           Test.QuickCheck.Arbitrary  (Arbitrary(..))
import           Test.QuickCheck.Gen        (Gen(..), listOf)

--------------------------------------------------------------------------------
-- | A JSON value represented as a Haskell value.
--
-- The 'Object''s payload is a key-value vector instead of a map, which parsed
-- directly from JSON document. This design choice has following advantages:
--
--    * Allow different strategies handling duplicated keys.
--    * Allow different 'Map' type to do further parsing, e.g. 'Z.Data.Vector.FlatMap'
--    * Roundtrip without touching the original key-value order.
--    * Save time if constructing map is not neccessary, e.g.
--      using a linear scan to find a key if only that key is needed.
--
data Value = Object {-# UNPACK #-} !(V.Vector (T.Text, Value))
           | Array  {-# UNPACK #-} !(V.Vector Value)
           | String {-# UNPACK #-} !T.Text
           | Number {-# UNPACK #-} !Scientific
           | Bool   !Bool
           | Null
         deriving (Eq, Ord, Show, Typeable, Generic)
         deriving anyclass Print

instance NFData Value where
    {-# INLINE rnf #-}
    rnf (Object o) = rnf o
    rnf (Array  a) = rnf a
    rnf (String s) = rnf s
    rnf (Number n) = rnf n
    rnf (Bool   b) = rnf b
    rnf Null = ()

instance Arbitrary Value where
    -- limit maximum depth of JSON document, otherwise it's too slow to run any tests
    arbitrary = arbitraryValue 0 4
      where
        arbitraryValue :: Int -> Int -> Gen Value
        arbitraryValue d s = do
            i <- arbitrary :: Gen Word
            case (i `mod` 6) of
                0 -> if d < s then Object . V.pack <$> listOf (arbitraryKV (d+1) s)
                              else pure Null
                1 -> if d < s then Array . V.pack <$> listOf (arbitraryValue (d+1) s)
                              else pure Null
                2 -> String <$> arbitrary
                3 -> do
                    c <- arbitrary
                    e <- arbitrary
                    pure . Number $ scientific c e
                4 -> Bool <$> arbitrary
                _ -> pure Null

        arbitraryKV d s = (,) <$> arbitrary <*> arbitraryValue d s

    shrink (Object kvs) = snd <$> (V.unpack kvs)
    shrink (Array vs) = V.unpack vs
    shrink _          = []

-- | Lense for 'Array' element.
--
-- 1. return `Null` if 'Value' is not an 'Array' or index not exist.
-- 2. Modify will have no effect if 'Value' is not an 'Array' or index not exist.
--
nth :: Functor f => Int -> (Value -> f Value) -> Value -> f Value
{-# INLINABLE nth #-}
nth ix f (Array vs) | Just v <- vs `indexMaybe` ix =
    fmap (\ x -> Array (V.unsafeModifyIndex vs ix (const x))) (f v)
nth _ f v = fmap (const v) (f Null)

-- | Lense for 'Object' element
--
-- 1. return `Null` if 'Value' is not an 'Object' or key not exist.
-- 2. Modify will have no effect if 'Value' is not an 'Object' or key not exist.
-- 4. On duplicated keys prefer the last one.
--
key :: Functor f => T.Text -> (Value -> f Value) -> Value -> f Value
{-# INLINABLE key #-}
key k f (Object kvs) | (i, Just (_, v)) <- V.findR ((k ==) . fst) kvs =
    fmap (\ x -> Object (V.unsafeModifyIndex kvs i (const (k, x)))) (f v)
key _ f v = fmap (const v) (f Null)

-- | Parse 'Value' without consuming trailing bytes.
parseValue :: V.Bytes -> (V.Bytes, Either P.ParseError Value)
{-# INLINE parseValue #-}
parseValue = P.parse value

-- | Parse 'Value', and consume all trailing JSON white spaces, if there're
-- bytes left, parsing will fail.
parseValue' :: V.Bytes -> Either P.ParseError Value
{-# INLINE parseValue' #-}
parseValue' = P.parse' (value <* skipSpaces <* P.endOfInput)

--------------------------------------------------------------------------------

-- | The only valid whitespace in a JSON document is space, newline,
-- carriage pure, and tab.
skipSpaces :: P.Parser ()
{-# INLINE skipSpaces #-}
skipSpaces = P.skipWhile (\ w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)

-- | JSON 'Value' parser.
value :: P.Parser Value
{-# INLINABLE value #-}
value = do
    skipSpaces
    w <- P.peek
    case w of
        DOUBLE_QUOTE    -> P.skipWord8 *> (String <$> string_)
        CURLY_LEFT      -> P.skipWord8 *> (Object <$> object_)
        SQUARE_LEFT     -> P.skipWord8 *> (Array <$> array_)
        LETTER_f        -> P.bytes "false" $> (Bool False)
        LETTER_t        -> P.bytes "true" $> (Bool True)
        LETTER_n        -> P.bytes "null" $> Null
        _   | w >= 48 && w <= 57 || w == MINUS -> Number <$> P.scientific'
            | otherwise -> P.fail' "Z.Data.JSON.Value.value: not a valid json value"

-- | parse json array with leading SQUARE_LEFT.
array :: P.Parser (V.Vector Value)
{-# INLINE array #-}
array = P.word8 SQUARE_LEFT *> array_

-- | parse json array without leading SQUARE_LEFT.
array_ :: P.Parser (V.Vector Value)
{-# INLINABLE array_ #-}
array_ = do
    skipSpaces
    w <- P.peek
    if w == SQUARE_RIGHT
    then P.skipWord8 $> V.empty
    else loop [] 1
  where
    loop :: [Value] -> Int -> P.Parser (V.Vector Value)
    loop acc !n = do
        !v <- value
        skipSpaces
        let acc' = v:acc
        ch <- P.satisfy $ \w -> w == COMMA || w == SQUARE_RIGHT
        if ch == COMMA
        then skipSpaces *> loop acc' (n+1)
        else pure $! V.packRN n acc'  -- n start from 1, so no need to +1 here

-- | parse json array with leading 'CURLY_LEFT'.
object :: P.Parser (V.Vector (T.Text, Value))
{-# INLINE object #-}
object = P.word8 CURLY_LEFT *> object_

-- | parse json object without leading 'CURLY_LEFT'.
object_ :: P.Parser (V.Vector (T.Text, Value))
{-# INLINABLE object_ #-}
object_ = do
    skipSpaces
    w <- P.peek
    if w == CURLY_RIGHT
    then P.skipWord8 $> V.empty
    else loop [] 1
 where
    loop :: [(T.Text, Value)] -> Int -> P.Parser (V.Vector (T.Text, Value))
    loop acc !n = do
        !k <- string
        skipSpaces
        P.word8 COLON
        !v <- value
        skipSpaces
        let acc' = (k, v) : acc
        ch <- P.satisfy $ \w -> w == COMMA || w == CURLY_RIGHT
        if ch == COMMA
        then skipSpaces *> loop acc' (n+1)
        else pure $! V.packRN n acc'  -- n start from 1, so no need to +1 here

--------------------------------------------------------------------------------

string :: P.Parser T.Text
{-# INLINE string #-}
string = P.word8 DOUBLE_QUOTE *> string_

string_ :: P.Parser T.Text
{-# INLINE string_ #-}
string_ = do
    (bs, state) <- P.scanChunks 0 go
    let mt = case state .&. 0xFF of
            -- need escaping
            1 -> unsafeDupablePerformIO (do
                    let !len = V.length bs
                    (!pa, !len') <- allocPrimArrayUnsafe len (\ mba# ->
                        withPrimVectorUnsafe bs (decode_json_string mba#))
                    if len' >= 0
                    then pure (Just (T.Text (V.PrimVector pa 0 len')))  -- unescaping also validate utf8
                    else pure Nothing)
            3 -> Nothing    -- reject unescaped control characters
            _ -> T.validateMaybe bs
    case mt of
        Just t -> P.skipWord8 $> t
        _  -> P.fail' "Z.Data.JSON.Value.string_: utf8 validation or unescaping failed"
  where
    go :: Word32 -> V.Bytes -> Either Word32 (V.Bytes, V.Bytes, Word32)
    go !state v =
        case unsafeDupablePerformIO . withPrimUnsafe state $ \ ps ->
                withPrimVectorUnsafe v (find_json_string_end ps)
        of (state', len)
            | len >= 0 ->
                let !r = V.unsafeTake len v
                    !rest = V.unsafeDrop len v
                in Right (r, rest, state')
            | otherwise -> Left state'

foreign import ccall unsafe find_json_string_end :: MBA# Word32 -> BA# Word8 -> Int -> Int -> IO Int
foreign import ccall unsafe decode_json_string :: MBA# Word8 -> BA# Word8 -> Int -> Int -> IO Int

--------------------------------------------------------------------------------

-- | Convert IEEE float to scientific notition.
floatToScientific :: Float -> Scientific
{-# INLINE floatToScientific #-}
floatToScientific rf | rf < 0    = -(fromFloatingDigits (B.grisu3_sp (-rf)))
                     | rf == 0   = 0
                     | otherwise = fromFloatingDigits (B.grisu3_sp rf)

-- | Convert IEEE double to scientific notition.
doubleToScientific :: Double -> Scientific
{-# INLINE doubleToScientific #-}
doubleToScientific rf | rf < 0    = -(fromFloatingDigits (B.grisu3 (-rf)))
                      | rf == 0   = 0
                      | otherwise = fromFloatingDigits (B.grisu3 rf)

fromFloatingDigits :: ([Int], Int) -> Scientific
{-# INLINE fromFloatingDigits #-}
fromFloatingDigits (digits, e) = go digits 0 0
  where
    -- There's no way a float or double has more digits a 'Int64' can't handle
    go :: [Int] -> Int64 -> Int -> Scientific
    go []     !c !n = scientific (fromIntegral c) (e - n)
    go (d:ds) !c !n = go ds (c * 10 + fromIntegral d) (n + 1)
