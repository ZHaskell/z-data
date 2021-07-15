{-|
Module      : Z.Data.JSON.Base
Description : Fast JSON serialization/deserialization
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides 'Converter' to convert 'Value' to haskell data types, and various tools to help
user define 'JSON' instance. It's recommended to use "Z.Data.JSON" instead since it contain more instances.

-}

module Z.Data.JSON.Base
  ( -- * JSON Class
    JSON(..), Value(..), defaultSettings, Settings(..)
  , -- * Encode & Decode
    DecodeError
  , decode, decode', decodeText, decodeText'
  , P.ParseChunks, decodeChunk, decodeChunks
  , encode, encodeChunks, encodeText
  , prettyJSON, JB.prettyValue, prettyJSON', JB.prettyValue'
    -- * parse into JSON Value
  , JV.parseValue, JV.parseValue'
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
  , JB.kv, JB.kv'
  , JB.string
  , B.curly, B.square
  , commaSepList
  , commaSepVec
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char                      (ord)
import           Data.Fixed
import           Data.Hashable
import qualified Data.Foldable                  as Foldable
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.Map.Strict                as M
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as Set
import qualified Data.Tree                      as Tree
import           Data.Int
import           Data.Kind                      (Type)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.NonEmpty             as NonEmpty
import qualified Data.Primitive.ByteArray       as A
import qualified Data.Primitive.SmallArray      as A
import           Data.Primitive.Types           (Prim)
import           Data.Ratio                     (Ratio, denominator, numerator, (%))
import           Data.Scientific                (Scientific, base10Exponent, toBoundedInteger)
import qualified Data.Scientific                as Scientific
import           Data.Word
import           GHC.Exts                       (Proxy#, proxy#)
import           GHC.Generics
import           GHC.Natural
import qualified Z.Data.Array                   as A
import qualified Z.Data.Builder                 as B
import           Z.Data.Generics.Utils
import qualified Z.Data.JSON.Builder            as JB
import           Z.Data.JSON.Converter
import           Z.Data.JSON.Value              (Value (..))
import qualified Z.Data.JSON.Value              as JV
import qualified Z.Data.Parser                  as P
import qualified Z.Data.Text                    as T
import qualified Z.Data.Text.Base               as T
import qualified Z.Data.Text.Print              as T
import qualified Z.Data.Vector.Base             as V
import qualified Z.Data.Vector.Base64           as Base64
import qualified Z.Data.Vector.Extra            as V
import qualified Z.Data.Vector.FlatIntMap       as FIM
import qualified Z.Data.Vector.FlatIntSet       as FIS
import qualified Z.Data.Vector.FlatMap          as FM
import qualified Z.Data.Vector.FlatSet          as FS

--------------------------------------------------------------------------------

-- | Type class for encode & decode JSON.
class JSON a where
    fromValue :: Value -> Converter a
    default fromValue :: (Generic a, GFromValue (Rep a)) => Value -> Converter a
    fromValue v = to <$> gFromValue defaultSettings v
    {-# INLINABLE fromValue #-}

    toValue :: a -> Value
    default toValue :: (Generic a, GToValue (Rep a)) => a -> Value
    toValue = gToValue defaultSettings . from
    {-# INLINABLE toValue #-}

    encodeJSON :: a -> B.Builder ()
    default encodeJSON :: (Generic a, GEncodeJSON (Rep a)) => a -> B.Builder ()
    encodeJSON = gEncodeJSON defaultSettings . from
    {-# INLINABLE encodeJSON #-}

--------------------------------------------------------------------------------

-- There're two possible failures here:
--
--   * 'P.ParseError' is an error during parsing bytes to 'Value'.
--   * 'ConvertError' is an error when converting 'Value' to target data type.
type DecodeError = Either P.ParseError ConvertError

-- | Decode a JSON doc, only trailing JSON whitespace are allowed.
decodeText' :: JSON a => T.Text -> Either DecodeError a
{-# INLINE decodeText' #-}
decodeText' = decode' . T.getUTF8Bytes

-- | Decode a JSON text, return any trailing text.
decodeText :: JSON a => T.Text -> (T.Text, Either DecodeError a)
{-# INLINE decodeText #-}
decodeText t =
    let (rest, r) = decode (T.getUTF8Bytes t)
    in (T.Text rest, r) -- JSON parser consume bytes in unit of UTF8 codepoint

-- | Decode a JSON doc, only trailing JSON whitespace are allowed.
decode' :: JSON a => V.Bytes -> Either DecodeError a
{-# INLINE decode' #-}
decode' bs = case P.parse' (JV.value <* JV.skipSpaces <* P.endOfInput) bs of
    Left pErr -> Left (Left pErr)
    Right v -> case convertValue v of
        Left cErr -> Left (Right cErr)
        Right r   -> Right r

-- | Decode a JSON bytes, return any trailing bytes.
decode :: JSON a => V.Bytes -> (V.Bytes, Either DecodeError a)
{-# INLINE decode #-}
decode bs = case P.parse JV.value bs of
    (bs', Left pErr) -> (bs', Left (Left pErr))
    (bs', Right v) -> case convertValue v of
        Left cErr -> (bs', Left (Right cErr))
        Right r   -> (bs', Right r)

-- | Decode a JSON doc chunk.
decodeChunk :: JSON a => V.Bytes -> P.Result DecodeError a
{-# INLINE decodeChunk #-}
decodeChunk bs = loop (P.parseChunk JV.value bs)
  where
    loop r = do
        case r of
            P.Success v rest ->
                case convertValue v of
                    Left cErr -> P.Failure (Right cErr) rest
                    Right r'  -> P.Success r' rest
            P.Failure e rest -> P.Failure (Left e) rest
            P.Partial f' -> P.Partial (loop . f')

-- | Decode JSON doc chunks, return trailing bytes.
decodeChunks :: (JSON a, Monad m) => P.ParseChunks m DecodeError a
{-# INLINE decodeChunks #-}
decodeChunks = P.parseChunks decodeChunk

-- | Directly encode data to JSON bytes.
--
-- This function use 'B.buildWith' 'V.smallChunkSize' to balance common use case, if you need fine tuning on memory usage,
-- please use 'B.buildWith' and a custom initial chunk size with 'encodeJSON'.
encode :: JSON a => a -> V.Bytes
{-# INLINE encode #-}
encode = B.buildWith V.smallChunkSize . encodeJSON

-- | Encode data to JSON bytes chunks.
encodeChunks :: JSON a => a -> [V.Bytes]
{-# INLINE encodeChunks #-}
encodeChunks = B.buildChunks . encodeJSON

-- | Text version 'encode'.
encodeText :: JSON a => a -> T.Text
{-# INLINE encodeText #-}
encodeText = T.Text . encode

-- | Run a 'Converter' with input value.
convertValue :: (JSON a) => Value -> Either ConvertError a
{-# INLINE convertValue #-}
convertValue = convert fromValue

-- | Pretty a 'JSON' data with 'JB.prettyValue'.
prettyJSON :: JSON a => a -> B.Builder ()
{-# INLINE prettyJSON #-}
prettyJSON = JB.prettyValue . toValue

-- | Pretty a 'JSON' data with 'JB.prettyValue\''.
prettyJSON' :: JSON a
            => Int   -- ^ indentation per level
            -> Int   -- ^ initial indentation
            -> a
            -> B.Builder ()
{-# INLINE prettyJSON' #-}
prettyJSON' i ii = JB.prettyValue' i ii . toValue

--------------------------------------------------------------------------------

-- | Produce an error message like @converting XXX failed, expected XXX, encountered XXX@.
typeMismatch :: T.Text     -- ^ The name of the type you are trying to convert.
             -> T.Text     -- ^ The JSON value type you expecting to meet.
             -> Value      -- ^ The actual value encountered.
             -> Converter a
{-# INLINABLE typeMismatch #-}
typeMismatch name expected v =
    fail' $ T.concat ["converting ", name, " failed, expected ", expected, ", encountered ", actual]
  where
    actual = case v of
        Object _ -> "Object"
        Array _  -> "Array"
        String _ -> "String"
        Number _ -> "Number"
        Bool _   -> "Boolean"
        _        -> "Null"

fromNull :: T.Text -> a -> Value -> Converter a
{-# INLINE fromNull #-}
fromNull _ a Null = pure a
fromNull c _ v    = typeMismatch c "Null" v

withBool :: T.Text -> (Bool -> Converter a) -> Value ->  Converter a
{-# INLINE withBool #-}
withBool _    f (Bool x) = f x
withBool name _ v        = typeMismatch name "Bool" v

-- | @'withScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Z.Data.JSON.Value.Number' and fails using 'typeMismatch'
-- otherwise.
--
-- /Warning/: If you are converting from a scientific to an unbounded
-- type such as 'Integer' you may want to add a restriction on the
-- size of the exponent (see 'withBoundedScientific') to prevent
-- malicious input from filling up the memory of the target system.
--
-- ==== Error message example
--
-- > withScientific "MyType" f (String "oops")
-- > -- Error: "converting MyType failed, expected Number, but encountered String"
withScientific :: T.Text -> (Scientific -> Converter a) -> Value ->  Converter a
{-# INLINE withScientific #-}
withScientific _    f (Number x) = f x
withScientific name _ v          = typeMismatch name "Number" v

-- | @'withRealFloat' try to convert floating number with following rules:
--
--   * Use @±Infinity@ to represent out of range numbers.
--   * Convert @Null@ as @NaN@
--
withRealFloat :: RealFloat a => T.Text -> (a -> Converter r) -> Value -> Converter r
{-# INLINE withRealFloat #-}
withRealFloat _    f (Number s) = f (Scientific.toRealFloat s)
withRealFloat _    f Null       = f (0/0)
withRealFloat name _ v          = typeMismatch name "Number or Null" v

-- | @'withBoundedScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Number' with exponent less than or equal to 1024.
withBoundedScientific :: T.Text -> (Scientific -> Converter a) -> Value ->  Converter a
{-# INLINE withBoundedScientific #-}
withBoundedScientific name f (Number x)
    | e <= 1024 = f x
    | otherwise = fail' . B.unsafeBuildText $ do
        "converting "
        T.text name
        " failed, found a number with exponent "
        T.int e
        ", but it must not be greater than 1024"
  where e = base10Exponent x
withBoundedScientific name _ v = typeMismatch name "Number" v

-- | @'withBoundedScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Number' and value is within @minBound ~ maxBound@.
withBoundedIntegral :: (Bounded a, Integral a) => T.Text -> (a -> Converter r) -> Value -> Converter r
{-# INLINE withBoundedIntegral #-}
withBoundedIntegral name f (Number x) =
    case toBoundedInteger x of
        Just i -> f i
        _      -> fail' . B.unsafeBuildText $ do
            "converting "
            T.text name
            "failed, value is either floating or will cause over or underflow: "
            T.scientific x
withBoundedIntegral name _ v = typeMismatch name "Number" v

withText :: T.Text -> (T.Text -> Converter a) -> Value -> Converter a
{-# INLINE withText #-}
withText _    f (String x) = f x
withText name _ v          = typeMismatch name "String" v

withArray :: T.Text -> (V.Vector Value -> Converter a) -> Value -> Converter a
{-# INLINE withArray #-}
withArray _ f (Array arr) = f arr
withArray name _ v        = typeMismatch name "Array" v

-- | Directly use 'Object' as key-values for further converting.
withKeyValues :: T.Text -> (V.Vector (T.Text, Value) -> Converter a) -> Value -> Converter a
{-# INLINE withKeyValues #-}
withKeyValues _    f (Object kvs) = f kvs
withKeyValues name _ v            = typeMismatch name "Object" v

-- | Take a 'Object' as an 'FM.FlatMap T.Text Value', on key duplication prefer first one.
withFlatMap :: T.Text -> (FM.FlatMap T.Text Value -> Converter a) -> Value -> Converter a
{-# INLINE withFlatMap #-}
withFlatMap _    f (Object obj) = f (FM.packVector obj)
withFlatMap name _ v            = typeMismatch name "Object" v

-- | Take a 'Object' as an 'FM.FlatMap T.Text Value', on key duplication prefer last one.
withFlatMapR :: T.Text -> (FM.FlatMap T.Text Value -> Converter a) -> Value -> Converter a
{-# INLINE withFlatMapR #-}
withFlatMapR _    f (Object obj) = f (FM.packVectorR obj)
withFlatMapR name _ v            = typeMismatch name "Object" v

-- | Take a 'Object' as an 'HM.HashMap T.Text Value', on key duplication prefer first one.
withHashMap :: T.Text -> (HM.HashMap T.Text Value -> Converter a) -> Value -> Converter a
{-# INLINE withHashMap #-}
withHashMap _    f (Object obj) = f (HM.fromList (V.unpackR obj))
withHashMap name _ v            = typeMismatch name "Object" v

-- | Take a 'Object' as an 'HM.HashMap T.Text Value', on key duplication prefer last one.
withHashMapR :: T.Text -> (HM.HashMap T.Text Value -> Converter a) -> Value -> Converter a
{-# INLINE withHashMapR #-}
withHashMapR _    f (Object obj) = f (HM.fromList (V.unpack obj))
withHashMapR name _ v            = typeMismatch name "Object" v

-- | Decode a nested JSON-encoded string.
withEmbeddedJSON :: T.Text                  -- ^ data type name
                 -> (Value -> Converter a)     -- ^ a inner converter which will get the converted 'Value'.
                 -> Value -> Converter a       -- a converter take a JSON String
{-# INLINABLE withEmbeddedJSON #-}
withEmbeddedJSON _ innerConverter (String txt) = Converter (\ kf k ->
        case decode' (T.getUTF8Bytes txt) of
            Right v -> runConverter (innerConverter v) (\ paths msg -> kf (Embedded:paths) msg) k
            Left (Left pErr) -> kf [] (T.intercalate ", " ("parsing embeded JSON failed ": pErr))
            _                -> error "Z.JSON.Base: impossible, converting to Value should not fail")
withEmbeddedJSON name _ v = typeMismatch name "String" v

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: (JSON a) => FM.FlatMap T.Text Value -> T.Text -> Converter a
{-# INLINE (.:) #-}
(.:) = convertField fromValue

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or fail if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (JSON a) => FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE (.:?) #-}
(.:?) = convertFieldMaybe fromValue

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or fail if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to convert 'Null' the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: (JSON a) => FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE (.:!) #-}
(.:!) = convertFieldMaybe' fromValue

convertField :: (Value -> Converter a)  -- ^ the field converter (value part of a key value pair)
           -> FM.FlatMap T.Text Value -> T.Text -> Converter a
{-# INLINE convertField #-}
convertField p obj key = case FM.lookup key obj of
    Just v -> p v <?> Key key
    _      -> fail' (T.concat $ ["key ", key, " not present"])

-- | Variant of '.:?' with explicit converter function.
convertFieldMaybe :: (Value -> Converter a) -> FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE convertFieldMaybe #-}
convertFieldMaybe p obj key = case FM.lookup key obj of
    Just Null -> pure Nothing
    Just v    -> Just <$> p v <?> Key key
    _         -> pure Nothing

-- | Variant of '.:!' with explicit converter function.
convertFieldMaybe' :: (Value -> Converter a) -> FM.FlatMap T.Text Value -> T.Text -> Converter (Maybe a)
{-# INLINE convertFieldMaybe' #-}
convertFieldMaybe' p obj key = case FM.lookup key obj of
    Just v -> Just <$> p v <?> Key key
    _      -> pure Nothing

--------------------------------------------------------------------------------

-- | Use @,@ as separator to connect list of builders.
commaSepList :: JSON a => [a] -> B.Builder ()
{-# INLINE commaSepList #-}
commaSepList = B.intercalateList B.comma encodeJSON

-- | Use @,@ as separator to connect a vector of builders.
commaSepVec :: (JSON a, V.Vec v a) => v a ->  B.Builder ()
{-# INLINE commaSepVec #-}
commaSepVec = B.intercalateVec B.comma encodeJSON

-- | A newtype for 'B.Builder', whose semigroup's instance is to connect two builder with 'B.comma'.
newtype KVItem = KVItem (B.Builder ())

instance Semigroup KVItem where
    {-# INLINE (<>) #-}
    KVItem a <> KVItem b = KVItem (a >> B.comma >> b)

-- | Connect key and value to a 'KVItem' using 'B.colon', key will be escaped.
(.!) :: JSON v => T.Text -> v -> KVItem
{-# INLINE (.!) #-}
k .! v = KVItem (k `JB.kv'` encodeJSON v)
infixr 8 .!

-- | Add curly for comma connected 'KVItem's.
object' :: KVItem -> B.Builder ()
{-# INLINE object' #-}
object' (KVItem kvb) = B.curly kvb

-- | Connect key and value to a tuple to be used with 'object'.
(.=) :: JSON v => T.Text -> v -> (T.Text, Value)
{-# INLINE (.=) #-}
k .= v = let !v' = toValue v in  (k, v')
infixr 8 .=

-- | Alias for @Object . pack@.
object :: [(T.Text, Value)] -> Value
{-# INLINE object #-}
object = Object . V.pack

--------------------------------------------------------------------------------
-- | Generic encode/decode Settings
--
-- There should be no control characters in formatted texts since we don't escaping those
-- field names or constructor names ('defaultSettings' relys on Haskell's lexical property).
-- Otherwise 'encodeJSON' will output illegal JSON string.
data Settings = Settings
    { fieldFmt  :: String -> T.Text -- ^ format field labels
    , constrFmt :: String -> T.Text -- ^ format constructor names
    , missingKeyAsNull :: Bool      -- ^ take missing field as 'Null'?
    }

-- | @Settings T.pack T.pack False@
defaultSettings :: Settings
{-# INLINE defaultSettings #-}
defaultSettings = Settings T.pack T.pack False

--------------------------------------------------------------------------------
-- GToValue
--------------------------------------------------------------------------------

class GToValue f where
    gToValue :: Settings -> f a -> Value

--------------------------------------------------------------------------------
-- Selectors

type family Field f where
    Field (a :*: b) = Field a
    Field (S1 (MetaSel Nothing u ss ds) f) = Value
    Field (S1 (MetaSel (Just l) u ss ds) f) = (T.Text, Value)

class GWriteFields f where
    gWriteFields :: Settings -> A.SmallMutableArray s (Field f) -> Int -> f a -> ST s ()

instance (ProductSize a, GWriteFields a, GWriteFields b, Field a ~ Field b) => GWriteFields (a :*: b) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx (a :*: b) = do
        gWriteFields s marr idx a
        gWriteFields s marr (idx + productSize (proxy# :: Proxy# a)) b

instance (GToValue f) => GWriteFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx (M1 x) = A.writeSmallArray marr idx (gToValue s x)

instance (GToValue f, Selector (MetaSel (Just l) u ss ds)) => GWriteFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx m1@(M1 x) = A.writeSmallArray marr idx ((fieldFmt s) (selName m1), gToValue s x)

instance (GToValue f, Selector (MetaSel (Just l) u ss ds)) => GToValue (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gToValue #-}
    gToValue s m1@(M1 x) =
        let !k = fieldFmt s $ selName m1
            !v = gToValue s x
        in Object (V.singleton (k, v))

instance GToValue f => GToValue (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gToValue #-}
    gToValue s (M1 x) = gToValue s x

instance JSON a => GToValue (K1 i a) where
    {-# INLINE gToValue #-}
    gToValue _ (K1 x) = toValue x

class GMergeFields f where
    gMergeFields :: Proxy# f -> A.SmallMutableArray s (Field f) -> ST s Value

instance GMergeFields a => GMergeFields (a :*: b) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ = gMergeFields (proxy# :: Proxy# a)

instance GMergeFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ marr = do
        arr <- A.unsafeFreezeSmallArray marr
        let l = A.sizeofSmallArray arr
        pure (Array (V.Vector arr 0 l))

instance GMergeFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ marr = do
        arr <- A.unsafeFreezeSmallArray marr
        let l = A.sizeofSmallArray arr
        pure (Object (V.Vector arr 0 l))

--------------------------------------------------------------------------------
-- Constructors

class GConstrToValue f where
    gConstrToValue :: Bool -> Settings -> f a -> Value

instance GConstrToValue V1 where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ _ _ = error "Z.Data.JSON.Base: empty data type"

instance (GConstrToValue f, GConstrToValue g) => GConstrToValue (f :+: g) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ s (L1 x) = gConstrToValue True s x
    gConstrToValue _ s (R1 x) = gConstrToValue True s x

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrToValue (C1 c U1) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ s (M1 _) = String . constrFmt s $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GToValue (S1 sc f)) => GConstrToValue (C1 c (S1 sc f)) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue False s (M1 x) = gToValue s x
    gConstrToValue True s (M1 x) =
        let !k = constrFmt s $ conName @c undefined
            !v = gToValue s x
        in Object (V.singleton (k, v))

-- | Constructor with multiple payloads
instance (ProductSize (a :*: b), GWriteFields (a :*: b), GMergeFields (a :*: b), Constructor c)
    => GConstrToValue (C1 c (a :*: b)) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue False s (M1 x) = runST (do
        marr <- A.newSmallArray (productSize (proxy# :: Proxy# (a :*: b))) undefined
        gWriteFields s marr 0 x
        gMergeFields (proxy# :: Proxy# (a :*: b)) marr)
    gConstrToValue True s (M1 x) =
        let !k = constrFmt s $ conName @c undefined
            !v = runST (do
                    marr <- A.newSmallArray (productSize (proxy# :: Proxy# (a :*: b))) undefined
                    gWriteFields s marr 0 x
                    gMergeFields (proxy# :: Proxy# (a :*: b)) marr)
        in Object (V.singleton (k, v))

--------------------------------------------------------------------------------
-- Data types
instance GConstrToValue f => GToValue (D1 c f) where
    {-# INLINE gToValue #-}
    gToValue s (M1 x) = gConstrToValue False s x

--------------------------------------------------------------------------------
-- JSON
--------------------------------------------------------------------------------

class GEncodeJSON f where
    gEncodeJSON :: Settings -> f a -> B.Builder ()

--------------------------------------------------------------------------------
-- Selectors

instance (GEncodeJSON f, Selector (MetaSel (Just l) u ss ds)) => GEncodeJSON (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s m1@(M1 x) = (fieldFmt s $ selName m1) `JB.kv` gEncodeJSON s x

instance GEncodeJSON f => GEncodeJSON (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (M1 x) = gEncodeJSON s x

instance (GEncodeJSON a, GEncodeJSON b) => GEncodeJSON (a :*: b) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (a :*: b) = gEncodeJSON s a >> B.comma >> gEncodeJSON s b

instance JSON a => GEncodeJSON (K1 i a) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON _ (K1 x) = encodeJSON x

class GAddPunctuation (f :: Type -> Type) where
    gAddPunctuation :: Proxy# f -> B.Builder () -> B.Builder ()

instance GAddPunctuation a => GAddPunctuation (a :*: b) where
    {-# INLINE gAddPunctuation #-}
    gAddPunctuation _ = gAddPunctuation (proxy# :: Proxy# a)

instance GAddPunctuation (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gAddPunctuation #-}
    gAddPunctuation _ b = B.square b

instance GAddPunctuation (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gAddPunctuation #-}
    gAddPunctuation _ b = B.curly b

--------------------------------------------------------------------------------
-- Constructors

class GConstrEncodeJSON f where
    gConstrEncodeJSON :: Bool -> Settings -> f a -> B.Builder ()

instance GConstrEncodeJSON V1 where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON _ _ _ = error "Z.Data.JSON.Base: empty data type"

instance (GConstrEncodeJSON f, GConstrEncodeJSON g) => GConstrEncodeJSON (f :+: g) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON _ s (L1 x) = gConstrEncodeJSON True s x
    gConstrEncodeJSON _ s (R1 x) = gConstrEncodeJSON True s x

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrEncodeJSON (C1 c U1) where
    {-# INLINE gConstrEncodeJSON #-}
    -- There should be no chars need escaping in constructor name
    gConstrEncodeJSON _ s (M1 _) = B.quotes $
        B.text . constrFmt s $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GEncodeJSON (S1 (MetaSel Nothing u ss ds) f))
    => GConstrEncodeJSON (C1 c (S1 (MetaSel Nothing u ss ds) f)) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON False s (M1 x) = gEncodeJSON s x
    gConstrEncodeJSON True s (M1 x) = B.curly $ do
        (constrFmt s $ conName @c undefined) `JB.kv` gEncodeJSON s x

instance (Constructor c, GEncodeJSON (S1 (MetaSel (Just l) u ss ds) f))
    => GConstrEncodeJSON (C1 c (S1 (MetaSel (Just l) u ss ds) f)) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON False s (M1 x) = B.curly (gEncodeJSON s x)
    gConstrEncodeJSON True s (M1 x) = B.curly $ do
        (constrFmt s $ conName @c undefined) `JB.kv` B.curly (gEncodeJSON s x)

-- | Constructor with multiple payloads
instance (GEncodeJSON (a :*: b), GAddPunctuation (a :*: b), Constructor c)
    => GConstrEncodeJSON (C1 c (a :*: b)) where
    {-# INLINE gConstrEncodeJSON #-}
    gConstrEncodeJSON False s (M1 x) = gAddPunctuation (proxy# :: Proxy# (a :*: b)) (gEncodeJSON s x)
    gConstrEncodeJSON True s (M1 x) = B.curly $ do
        (constrFmt s $ conName @c @_ @_ @_ undefined) `JB.kv`
            gAddPunctuation (proxy# :: Proxy# (a :*: b)) (gEncodeJSON s x)

--------------------------------------------------------------------------------
-- Data types
instance GConstrEncodeJSON f => GEncodeJSON (D1 c f) where
    {-# INLINE gEncodeJSON #-}
    gEncodeJSON s (M1 x) = gConstrEncodeJSON False s x

--------------------------------------------------------------------------------
-- GFromValue
--------------------------------------------------------------------------------

class GFromValue f where
    gFromValue :: Settings -> Value -> Converter (f a)

--------------------------------------------------------------------------------
-- Selectors

type family LookupTable f where
    LookupTable (a :*: b) = LookupTable a
    LookupTable (S1 (MetaSel Nothing u ss ds) f) = V.Vector Value
    LookupTable (S1 (MetaSel (Just l) u ss ds) f) = FM.FlatMap T.Text Value

class GFromFields f where
    gFromFields :: Settings -> LookupTable f -> Int -> Converter (f a)

instance (ProductSize a, GFromFields a, GFromFields b, LookupTable a ~ LookupTable b)
    => GFromFields (a :*: b) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        !a <- gFromFields s v idx
        !b <- gFromFields s v (idx + productSize (proxy# :: Proxy# a))
        pure (a :*: b)

instance (GFromValue f) => GFromFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        v' <- V.unsafeIndexM v idx
        M1 <$> gFromValue s v' <?> Index idx

instance (GFromValue f, Selector (MetaSel (Just l) u ss ds)) => GFromFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v _ = do
        case FM.lookup fn v of
            Just v' -> M1 <$> gFromValue s v' <?> Key fn
            _ | missingKeyAsNull s -> M1 <$> gFromValue s Null <?> Key fn
              | otherwise -> fail' ("Z.Data.JSON.Base: missing field " <>  fn)
      where
        fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))

instance GFromValue f => GFromValue (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromValue #-}
    gFromValue s x = M1 <$> gFromValue s x

instance (GFromValue f, Selector (MetaSel (Just l) u ss ds)) => GFromValue (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFromValue #-}
    gFromValue s (Object v) = do
        case FM.lookup fn (FM.packVectorR v) of
            Just v' -> M1 <$> gFromValue s v' <?> Key fn
            _ | missingKeyAsNull s -> M1 <$> gFromValue s Null <?> Key fn
              | otherwise -> fail' ("Z.Data.JSON.Base: missing field " <>  fn)
      where fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))
    gFromValue s v = typeMismatch ("field " <> fn) "Object" v <?> Key fn
      where fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))

instance JSON a => GFromValue (K1 i a) where
    {-# INLINE gFromValue #-}
    gFromValue _ x = K1 <$> fromValue x

class GBuildLookup f where
    gBuildLookup :: Proxy# f -> Int -> T.Text -> Value -> Converter (LookupTable f)

instance (GBuildLookup a, GBuildLookup b) => GBuildLookup (a :*: b) where
    {-# INLINE gBuildLookup #-}
    gBuildLookup _ siz = gBuildLookup (proxy# :: Proxy# a) siz

instance GBuildLookup (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gBuildLookup #-}
    gBuildLookup _ siz name (Array v)
        -- we have to check size here to use 'unsafeIndexM' later
        | siz' /= siz = fail' . B.unsafeBuildText $ do
            "converting "
            T.text name
            " failed, product size mismatch, expected "
            T.int siz
            ", get"
            T.int siz'
        | otherwise = pure v
      where siz' = V.length v
    gBuildLookup _ _   name x         = typeMismatch name "Array" x

instance GBuildLookup (S1 ((MetaSel (Just l) u ss ds)) f) where
    {-# INLINE gBuildLookup #-}
    -- we don't check size, so that duplicated keys are preserved
    gBuildLookup _ _ _ (Object v) = pure $! FM.packVectorR v
    gBuildLookup _ _ name x       = typeMismatch name "Object" x

--------------------------------------------------------------------------------
-- Constructors

class GConstrFromValue f where
    gConstrFromValue :: Bool    -- ^ Is this a sum type(more than one constructor)?
                     -> Settings -> Value -> Converter (f a)

instance GConstrFromValue V1 where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ _ _ = error "Z.Data.JSON.Base: empty data type"

instance (GConstrFromValue f, GConstrFromValue g) => GConstrFromValue (f :+: g) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ s x = (L1 <$> gConstrFromValue True s x) <|> (R1 <$> gConstrFromValue True s x)

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrFromValue (C1 c U1) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ s (String x)
        | cn == x   = pure (M1 U1)
        | otherwise = fail' . T.concat $ ["converting ", cn', "failed, unknown constructor name ", x]
      where cn = constrFmt s $ conName (undefined :: t c U1 a)
            cn' = T.pack $ conName (undefined :: t c U1 a)
    gConstrFromValue _ _ v = typeMismatch cn' "String" v
      where cn' = T.pack $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GFromValue (S1 sc f)) => GConstrFromValue (C1 c (S1 sc f)) where
    {-# INLINE gConstrFromValue #-}
    -- | Single constructor
    gConstrFromValue False s x = M1 <$> gFromValue s x
    gConstrFromValue True s x = case x of
        Object v -> case V.indexM v 0 of
            Just (k, v') | k == cn -> M1 <$> gFromValue s v' <?> Key cn
            _                      -> fail' .T.concat $ ["converting ", cn', " failed, constructor not found"]
        _ ->  typeMismatch cn' "Object" x
      where cn = constrFmt s $ conName @c undefined
            cn' = T.pack $ conName @c undefined

-- | Constructor with multiple payloads
instance (ProductSize (a :*: b), GFromFields (a :*: b), GBuildLookup (a :*: b), Constructor c)
    => GConstrFromValue (C1 c (a :*: b)) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue False s x = do
        t <- gBuildLookup p (productSize p) cn' x
        M1 <$> gFromFields s t 0
      where cn' = T.pack $ conName @c undefined
            p = proxy# :: Proxy# (a :*: b)
    gConstrFromValue True s x = case x of
        Object v -> case V.indexM v 0 of
            Just (k, v') | k == cn -> do t <- gBuildLookup p (productSize p) cn' v'
                                         M1 <$> gFromFields s t 0
            _                      -> fail' .T.concat $ ["converting ", cn', " failed, constructor not found"]
        _ ->  typeMismatch cn' "Object" x
      where cn = constrFmt s $ conName @c undefined
            cn' = T.pack $ conName @c undefined
            p = proxy# :: Proxy# (a :*: b)

--------------------------------------------------------------------------------
-- Data types
instance GConstrFromValue f => GFromValue (D1 c f) where
    {-# INLINE gFromValue #-}
    gFromValue s x = M1 <$> gConstrFromValue False s x

--------------------------------------------------------------------------------
-- Built-in Instances
--------------------------------------------------------------------------------

instance JSON Value   where
    {-# INLINE fromValue #-}; fromValue = pure;
    {-# INLINE toValue #-}; toValue = id;
    {-# INLINE encodeJSON #-}; encodeJSON = JB.value;

instance JSON T.Text   where
    {-# INLINE fromValue #-}; fromValue = withText "Text" pure;
    {-# INLINE toValue #-}; toValue = String;
    {-# INLINE encodeJSON #-}; encodeJSON = JB.string;

-- | Note this instance doesn't reject large input
instance JSON Scientific where
    {-# INLINE fromValue #-}; fromValue = withScientific "Scientific" pure;
    {-# INLINE toValue #-}; toValue = Number;
    {-# INLINE encodeJSON #-}; encodeJSON = B.scientific';

-- | default instance prefer later key
instance JSON a => JSON (FM.FlatMap T.Text a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Z.Data.Vector.FlatMap.FlatMap"
        (FM.traverseWithKey $ \ k v -> fromValue v <?> Key k)
    {-# INLINE toValue #-}
    toValue = Object . FM.sortedKeyValues . FM.map' toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.object' encodeJSON . FM.sortedKeyValues

instance (Ord a, JSON a) => JSON (FS.FlatSet a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.FlatSet.FlatSet" $ \ vs ->
        FS.packRN (V.length vs) <$>
            (zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs))
    {-# INLINE toValue #-}
    toValue = Array . V.map' toValue . FS.sortedValues
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.array' encodeJSON . FS.sortedValues

-- | default instance prefer later key
instance JSON a => JSON (HM.HashMap T.Text a) where
    {-# INLINE fromValue #-}
    fromValue = withHashMapR "Data.HashMap.HashMap"
        (HM.traverseWithKey $ \ k v -> fromValue v <?> Key k)
    {-# INLINE toValue #-}
    toValue = Object . V.pack . HM.toList . HM.map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.curly . B.intercalateList B.comma (\ (k, v) -> k `JB.kv'` encodeJSON v) . HM.toList

instance JSON a => JSON (M.Map T.Text a) where
    {-# INLINE fromValue #-}
    fromValue = withKeyValues "Data.Map.Map" $
        (M.traverseWithKey (\ k v -> fromValue v <?> Key k)) . M.fromList . V.unpack
    {-# INLINE toValue #-}
    toValue = Object . V.pack . M.toList . M.map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.curly . B.intercalateList B.comma (\ (k, v) -> k `JB.kv'` encodeJSON v) . M.toList

instance JSON a => JSON (FIM.FlatIntMap a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Z.Data.Vector.FlatIntMap.FlatIntMap" $ \ m ->
        let kvs = FM.sortedKeyValues m
        in FIM.packVectorR <$> (forM kvs $ \ (k, v) -> do
            case P.parse' P.int (T.getUTF8Bytes k) of
                Right k' -> do
                    v' <- fromValue v <?> Key k
                    return (V.IPair k' v')
                _ -> fail' ("converting Z.Data.Vector.FlatIntMap.FlatIntMap failed, unexpected key " <> k))
    {-# INLINE toValue #-}
    toValue = Object . V.map' toKV . FIM.sortedKeyValues
      where toKV (V.IPair i x) = let !k = T.toText i
                                     !v = toValue x
                                 in (k, v)
    {-# INLINE encodeJSON #-}
    encodeJSON = B.curly . B.intercalateVec B.comma (\ (V.IPair i x) -> do
        B.quotes (B.int i)
        B.colon
        encodeJSON x) . FIM.sortedKeyValues

instance JSON a => JSON (IM.IntMap a) where
    {-# INLINE fromValue #-}
    fromValue = withKeyValues "Data.IntMap.IntMap" $ \ kvs ->
        IM.fromList <$> (forM (V.unpack kvs) $ \ (k, v) -> do
            case P.parse' P.int (T.getUTF8Bytes k) of
                Right k' -> do
                    !v' <- fromValue v <?> Key k
                    return (k', v')
                _ -> fail' ("converting Data.IntMap.IntMap failed, unexpected key " <> k))
    {-# INLINE toValue #-}
    toValue = Object . V.pack . map toKV . IM.toList
      where toKV (i, x) = let !k = T.toText i
                              !v = toValue x
                          in (k, v)
    {-# INLINE encodeJSON #-}
    encodeJSON = B.curly . B.intercalateList B.comma (\ (i, x) -> do
        B.quotes (B.int i)
        B.colon
        encodeJSON x) . IM.toList

instance JSON FIS.FlatIntSet where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.FlatIntSet.FlatIntSet" $ \ vs ->
        FIS.packRN (V.length vs) <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = toValue . FIS.sortedValues
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . FIS.sortedValues

instance JSON IS.IntSet where
    {-# INLINE fromValue #-}
    fromValue = withArray "Data.IntSet.IntSet" $ \ vs ->
        IS.fromList <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = toValue . IS.toList
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . IS.toList

instance (Ord a, JSON a) => JSON (Set.Set a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Data.Set.Set" $ \ vs ->
        Set.fromList <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = toValue . Set.toList
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . Set.toList

instance JSON a => JSON (Seq.Seq a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Data.Seq.Seq" $ \ vs ->
        Seq.fromList <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = toValue . Foldable.toList
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . Foldable.toList

instance JSON a => JSON (Tree.Tree a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Data.Tree" $ \obj -> do
        !n <- obj .: "rootLabel"
        !d <- obj .: "subForest"
        pure (Tree.Node n d)
    {-# INLINE toValue #-}
    toValue x = object [ "rootLabel" .= (Tree.rootLabel x) , "subForest" .= (Tree.subForest x) ]
    {-# INLINE encodeJSON #-}
    encodeJSON x = object' ( "rootLabel" .! (Tree.rootLabel x) <> "subForest" .! (Tree.subForest x) )

instance JSON a => JSON (A.Array a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Array.Array"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaSepVec

instance JSON a => JSON (A.SmallArray a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Array.SmallArray"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaSepVec

instance (Prim a, JSON a) => JSON (A.PrimArray a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Array.PrimArray"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaSepVec

instance JSON A.ByteArray where
    {-# INLINE fromValue #-}
    fromValue value = do
        (A.PrimArray ba# :: A.PrimArray Word8) <-
                withArray "Data.Primitive.ByteArray"
                    (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k) value
        return (A.ByteArray ba#)
    {-# INLINE toValue #-}
    toValue (A.ByteArray ba#) =
        Array (V.map toValue (A.PrimArray ba# :: A.PrimArray Word8))
    {-# INLINE encodeJSON #-}
    encodeJSON (A.ByteArray ba#) =
        B.square (commaSepVec (A.PrimArray ba# :: A.PrimArray Word8))

instance (A.PrimUnlifted a, JSON a) => JSON (A.UnliftedArray a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Array.UnliftedArray"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaSepVec

instance JSON a => JSON (V.Vector a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.Vector"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaSepVec

instance (Prim a, JSON a) => JSON (V.PrimVector a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.PrimVector"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaSepVec

-- | This is an INCOHERENT instance, encode binary data with base64 encoding.
instance {-# INCOHERENT #-} JSON V.Bytes where
    fromValue = withText "Z.Data.Vector.Bytes" $ \ t ->
        case Base64.base64Decode (T.getUTF8Bytes t) of
            Just bs -> pure bs
            Nothing -> fail' "illegal base64 encoding bytes"
    {-# INLINE toValue #-}
    toValue = String . Base64.base64EncodeText
    {-# INLINE encodeJSON #-}
    encodeJSON = B.quotes . Base64.base64EncodeBuilder

instance (Eq a, Hashable a, JSON a) => JSON (HS.HashSet a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.FlatSet.FlatSet" $ \ vs ->
        HS.fromList <$>
            (zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs))
    {-# INLINE toValue #-}
    toValue = toValue . HS.toList
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . HS.toList

instance JSON a => JSON [a] where
    {-# INLINE fromValue #-}
    fromValue = withArray "[a]" $ \ vs ->
        zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = Array . V.pack . map toValue
    {-# INLINE encodeJSON #-}
    encodeJSON = B.square . commaSepList

-- | This is an INCOHERENT instance, to provide JSON text encoding behaviour.
instance {-# INCOHERENT #-} JSON [Char] where
    {-# INLINE fromValue #-}
    fromValue = withText "String" (pure . T.unpack)
    {-# INLINE toValue #-}
    toValue = String . T.pack
    {-# INLINE encodeJSON #-}
    encodeJSON = JB.string . T.pack

instance JSON a => JSON (NonEmpty a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "NonEmpty" $ \ vs -> do
        l <- zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
        case l of (x:xs) -> pure (x :| xs)
                  _      -> fail' "unexpected empty array"
    {-# INLINE toValue #-}
    toValue = toValue . NonEmpty.toList
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . NonEmpty.toList

instance JSON Bool where
    {-# INLINE fromValue #-}; fromValue = withBool "Bool" pure;
    {-# INLINE toValue #-}; toValue = Bool;
    {-# INLINE encodeJSON #-}; encodeJSON True = "true"; encodeJSON _ = "false";

instance JSON Char where
    {-# INLINE fromValue #-}
    fromValue = withText "Char" $ \ t ->
        if (T.length t == 1)
        then pure (T.head t)
        else fail' (T.concat ["converting Char failed, expected a string of length 1"])
    {-# INLINE toValue #-}
    toValue = String . T.singleton
-- @
--    \'\\b\':  \"\\b\"
--    \'\\f\':  \"\\f\"
--    \'\\n\':  \"\\n\"
--    \'\\r\':  \"\\r\"
--    \'\\t\':  \"\\t\"
--    \'\"\':  \"\\\"\"
--    \'\\\':  \"\\\\\"
--    \'\DEL\':  \"\\u007f\"
--    other chars <= 0x1F: "\\u00xx"
-- @
    {-# INLINE encodeJSON #-}
    encodeJSON '\b' = "\"\\b\""
    encodeJSON '\f' = "\"\\f\""
    encodeJSON '\n' = "\"\\n\""
    encodeJSON '\r' = "\"\\r\""
    encodeJSON '\t' = "\"\\t\""
    encodeJSON '\"' = "\"\\\"\""
    encodeJSON '\\' = "\"\\\\\""
    encodeJSON c | c <= '\US' = "\"\\u00" >> B.hex (fromIntegral (ord c) :: Word8) >> B.char8 '\"'
                 | otherwise  = B.quotes (B.charUTF8 c)


instance JSON Double where
    {-# INLINE fromValue #-}; fromValue = withRealFloat "Double" pure;
    {-# INLINE toValue #-}; toValue = Number . JV.doubleToScientific;
    {-# INLINE encodeJSON #-}; encodeJSON = B.double;
instance JSON Float  where
    {-# INLINE fromValue #-}; fromValue = withRealFloat "Float" pure;
    {-# INLINE toValue #-}; toValue = Number . JV.floatToScientific;
    {-# INLINE encodeJSON #-}; encodeJSON = B.float;

#define INT_JSON_INSTANCE(typ) \
    instance JSON typ where \
        {-# INLINE fromValue #-}; fromValue = withBoundedIntegral " typ " pure; \
        {-# INLINE toValue #-}; toValue = Number . fromIntegral; \
        {-# INLINE encodeJSON #-}; encodeJSON = B.int;

INT_JSON_INSTANCE(Int)
INT_JSON_INSTANCE(Int8)
INT_JSON_INSTANCE(Int16)
INT_JSON_INSTANCE(Int32)
INT_JSON_INSTANCE(Int64)
INT_JSON_INSTANCE(Word)
INT_JSON_INSTANCE(Word8)
INT_JSON_INSTANCE(Word16)
INT_JSON_INSTANCE(Word32)
INT_JSON_INSTANCE(Word64)

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype 'Integer' and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance JSON Integer where
    {-# INLINE fromValue #-}
    fromValue = withBoundedScientific "Integer" $ \ n ->
        case Scientific.floatingOrInteger n :: Either Double Integer of
            Right x -> pure x
            Left _  -> fail' . B.unsafeBuildText $ do
                "converting Integer failed, unexpected floating number "
                T.scientific n
    {-# INLINE toValue #-}
    toValue = Number . fromIntegral
    {-# INLINE encodeJSON #-}
    encodeJSON = B.integer

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype 'Natural' and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance JSON Natural where
    {-# INLINE fromValue #-}
    fromValue = withBoundedScientific "Natural" $ \ n ->
        if n < 0
        then fail' . B.unsafeBuildText $ do
                "converting Natural failed, unexpected negative number "
                T.scientific n
        else case Scientific.floatingOrInteger n :: Either Double Natural of
            Right x -> pure x
            Left _  -> fail' . B.unsafeBuildText $ do
                "converting Natural failed, unexpected floating number "
                T.scientific n
    {-# INLINE toValue #-}
    toValue = Number . fromIntegral
    {-# INLINE encodeJSON #-}
    encodeJSON = B.integer . fromIntegral

instance JSON Ordering where
    {-# INLINE fromValue #-}
    fromValue = withText "Ordering" $ \ s ->
        case s of
            "LT" -> pure LT
            "EQ" -> pure EQ
            "GT" -> pure GT
            _ -> fail' . T.concat $ ["converting Ordering failed, unexpected ",
                                        s, " expected \"LT\", \"EQ\", or \"GT\""]
    {-# INLINE toValue #-}
    toValue LT = String "LT"
    toValue EQ = String "EQ"
    toValue GT = String "GT"
    {-# INLINE encodeJSON #-}
    encodeJSON LT = "\"LT\""
    encodeJSON EQ = "\"EQ\""
    encodeJSON GT = "\"GT\""

instance JSON () where
    {-# INLINE fromValue #-}
    fromValue = withArray "()" $ \ v ->
        if V.null v
        then pure ()
        else fail' "converting () failed, expected an empty array"
    {-# INLINE toValue #-}
    toValue () = Array V.empty
    {-# INLINE encodeJSON #-}
    encodeJSON () = "[]"

instance JSON a => JSON (Maybe a) where
    {-# INLINE fromValue #-}
    fromValue Null = pure Nothing
    fromValue v    = Just <$> fromValue v
    {-# INLINE toValue #-}
    toValue Nothing  = Null
    toValue (Just x) = toValue x
    {-# INLINE encodeJSON #-}
    encodeJSON Nothing  = "null"
    encodeJSON (Just x) = encodeJSON x

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype Ratio and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance (JSON a, Integral a) => JSON (Ratio a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Rational" $ \obj -> do
        !n <- obj .: "numerator"
        !d <- obj .: "denominator"
        if d == 0
        then fail' "Ratio denominator was 0"
        else pure (n % d)
    {-# INLINE toValue #-}
    toValue x = object [ "numerator" .= (numerator x) , "denominator" .= (denominator x) ]
    {-# INLINE encodeJSON #-}
    encodeJSON x = object' ( "numerator" .! (numerator x) <> "denominator" .! (denominator x) )

-- | This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype Fixed and provide your own instance using 'withScientific' if you want to allow larger inputs.
instance HasResolution a => JSON (Fixed a) where
    {-# INLINE fromValue #-}
    fromValue = withBoundedScientific "Fixed" (pure . realToFrac)
    {-# INLINE toValue #-}
    toValue = Number . realToFrac
    {-# INLINE encodeJSON #-}
    encodeJSON = B.scientific' . realToFrac
