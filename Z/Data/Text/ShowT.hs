{-|
Module      : Z.Data.Text.Builder
Description : UTF8 compatible builders.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Base on UTF8 compatible textual builders from 'Z.Data.Builder', we provide a newtype wrapper
'TextBuilder' which can be directly used to build 'Text'.

We also provide faster alternative to 'Show' class, i.e. 'ShowT', which also provides 'Generic'
based instances deriving.

-}

module Z.Data.Text.ShowT
  ( -- * ShowT class
    ShowT(..), showT, toBuilder, toBytes, toString
  -- * Textual Builder
  , TextBuilder
  , getBuilder
  , unsafeFromBuilder
  , buildText
  -- * Basic UTF8 builders
  , stringUTF8, charUTF8, string7, char7, text, escapeTextJSON
  -- * Numeric builders
  -- ** Integral type formatting
  , B.IFormat(..)
  , B.defaultIFormat
  , B.Padding(..)
  , int
  , intWith
  , integer
  -- ** Fixded size hexidecimal formatting
  , hex, heX
  -- ** IEEE float formating
  , B.FFormat(..)
  , double
  , doubleWith
  , float
  , floatWith
  , scientific
  , scientificWith
  -- * Builder helpers
  , paren, parenWhen, curly, square, angle, quotes, squotes, colon, comma, intercalateVec, intercalateList
  ) where

import           Control.Monad
import           Control.Monad.ST.Unsafe        (unsafeIOToST)
import qualified Data.Scientific                as Sci
import           Data.String
import           Data.Bits
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
import           Data.Proxy                     (Proxy(..))
import           Data.Ratio                     (Ratio, numerator, denominator)
import           Data.Tagged                    (Tagged (..))
import qualified Data.Semigroup                 as Semigroup
import           Data.Typeable
import           Foreign.C.Types
import           GHC.Exts
import           GHC.ForeignPtr
import           GHC.Generics
import           GHC.Natural
import           GHC.Stack
import           GHC.Word
import           Data.Version
import           System.Exit
import           Test.QuickCheck.Arbitrary (Arbitrary(..), CoArbitrary(..))
import           Data.Primitive.Types
import qualified Z.Data.Builder.Base            as B
import qualified Z.Data.Builder.Numeric         as B
import qualified Z.Data.Text.Base               as T
import           Z.Data.Text.Base               (Text(..))
import qualified Z.Data.Array                   as A
import qualified Z.Data.Vector.Base             as V

#define DOUBLE_QUOTE 34

-- | Buidlers which guarantee UTF-8 encoding, thus can be used to build
-- text directly.
--
-- Notes on 'IsString' instance: It's recommended to use 'IsString' instance, there's a rewrite rule to
-- turn encoding loop into a memcpy, which is much faster (the same rule also apply to 'stringUTF8').
-- Different from @Builder ()@, @TextBuilder ()@'s 'IsString' instance will give you desired UTF8 guarantees:
--
-- * @\NUL@ will be written directly as @\x00@.
--
-- * @\xD800@ ~ @\xDFFF@ will be replaced by replacement char.
--
newtype TextBuilder a = TextBuilder { getBuilder :: B.Builder a }
    deriving newtype (Functor, Applicative, Monad)

deriving newtype instance Semigroup (TextBuilder ())
deriving newtype instance Monoid (TextBuilder ())

instance (a ~ ()) => IsString (TextBuilder a) where
    {-# INLINE fromString #-}
    fromString = TextBuilder <$> B.stringUTF8

instance Arbitrary (TextBuilder ()) where
    arbitrary = TextBuilder . B.text <$> arbitrary
    shrink b = TextBuilder . B.text <$> shrink (buildText b)

instance CoArbitrary (TextBuilder ()) where
    coarbitrary = coarbitrary . buildText

instance Show (TextBuilder a) where
    show = show . buildText

instance ShowT (TextBuilder a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ b = quotes (void b)

-- | Build a 'Text' using 'TextBuilder', which provide UTF-8 encoding guarantee.
buildText :: TextBuilder a -> Text
{-# INLINE buildText #-}
buildText = Text . B.buildBytes . getBuilder

-- | Unsafely turn a 'B.Builder' into 'TextBuilder', thus it's user's responsibility to
-- ensure only UTF-8 complied bytes are written.
unsafeFromBuilder :: B.Builder a -> TextBuilder a
{-# INLINE unsafeFromBuilder #-}
unsafeFromBuilder = TextBuilder

--------------------------------------------------------------------------------

-- | Turn 'String' into 'TextBuilder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's. This function will be rewritten into a memcpy if possible, (running a fast UTF-8 validation at runtime first).
stringUTF8 :: String -> TextBuilder ()
{-# INLINE stringUTF8 #-}
stringUTF8 = TextBuilder . B.stringUTF8

-- | Turn 'Char' into 'TextBuilder' with UTF8 encoding
--
-- Illegal codepoints will be written as 'T.replacementChar's.
charUTF8 :: Char -> TextBuilder ()
{-# INLINE charUTF8 #-}
charUTF8 = TextBuilder . B.charUTF8

-- | Turn 'String' into 'TextBuilder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
string7 :: String -> TextBuilder ()
{-# INLINE string7 #-}
string7 = TextBuilder . B.string7

-- | Turn 'Char' into 'TextBuilder' with ASCII7 encoding
--
-- Codepoints beyond @'\x7F'@ will be chopped.
char7 :: Char -> TextBuilder ()
{-# INLINE char7 #-}
char7 = TextBuilder . B.char7

-- | Write UTF8 encoded 'T.Text' using 'Builder'.
--
-- Note, if you're trying to write string literals builders,
-- please open 'OverloadedStrings' and use 'Builder's 'IsString' instance,
-- it will be rewritten into a memcpy.
text :: T.Text -> TextBuilder ()
{-# INLINE text #-}
text = TextBuilder . B.text

--------------------------------------------------------------------------------

-- | @int = intWith defaultIFormat@
int :: (Integral a, Bounded a) => a -> TextBuilder ()
{-# INLINE int #-}
int = TextBuilder . B.int

-- | Format a 'Bounded' 'Integral' type like @Int@ or @Word16@ into decimal ascii digits.
intWith :: (Integral a, Bounded a)
        => B.IFormat
        -> a
        -> TextBuilder ()
{-# INLINE intWith #-}
intWith fmt x = TextBuilder $ B.intWith fmt x

-- | Format a 'Integer' into decimal ascii digits.
integer :: Integer -> TextBuilder ()
{-# INLINE integer #-}
integer = TextBuilder . B.integer

-- | Format a 'FiniteBits' 'Integral' type into hex nibbles.
hex :: (FiniteBits a, Integral a) => a -> TextBuilder ()
{-# INLINE hex #-}
hex = TextBuilder . B.hex

-- | The UPPERCASED version of 'hex'.
heX :: (FiniteBits a, Integral a) => a -> TextBuilder ()
{-# INLINE heX #-}
heX = TextBuilder . B.heX

-- | Decimal encoding of an IEEE 'Float'.
--
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
float :: Float -> TextBuilder ()
{-# INLINE float #-}
float = TextBuilder . B.float

-- | Format single-precision float using drisu3 with dragon4 fallback.
floatWith :: B.FFormat
          -> Maybe Int  -- ^ Number of decimal places to render.
          -> Float
          -> TextBuilder ()
{-# INLINE floatWith #-}
floatWith fmt ds x = TextBuilder (B.floatWith fmt ds x)


-- | Decimal encoding of an IEEE 'Double'.
--
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
double :: Double -> TextBuilder ()
{-# INLINE double #-}
double = TextBuilder . B.double

-- | Format double-precision float using drisu3 with dragon4 fallback.
doubleWith :: B.FFormat
           -> Maybe Int  -- ^ Number of decimal places to render.
           -> Double
           -> TextBuilder ()
{-# INLINE doubleWith #-}
doubleWith fmt ds x = TextBuilder (B.doubleWith fmt ds x)


-- | A @Builder@ which renders a scientific number to full
-- precision, using standard decimal notation for arguments whose
-- absolute value lies between @0.1@ and @9,999,999@, and scientific
-- notation otherwise.
scientific :: Sci.Scientific -> TextBuilder ()
{-# INLINE scientific #-}
scientific = TextBuilder . B.scientific

-- | Like 'scientific' but provides rendering options.
scientificWith :: B.FFormat
               -> Maybe Int  -- ^ Number of decimal places to render.
               -> Sci.Scientific
               -> TextBuilder ()
{-# INLINE scientificWith #-}
scientificWith fmt ds x = TextBuilder (B.scientificWith fmt ds x)

--------------------------------------------------------------------------------

-- | add @(...)@ to original builder.
paren :: TextBuilder () -> TextBuilder ()
{-# INLINE paren #-}
paren (TextBuilder b) = TextBuilder (B.paren b)

-- | Add "(..)" around builders when condition is met, otherwise add nothing.
--
-- This is useful when defining 'ShowT' instances.
parenWhen :: Bool -> TextBuilder () -> TextBuilder ()
{-# INLINE parenWhen #-}
parenWhen True b = paren b
parenWhen _    b = b

-- | add @{...}@ to original builder.
curly :: TextBuilder () -> TextBuilder ()
{-# INLINE curly #-}
curly (TextBuilder b) = TextBuilder (B.curly b)

-- | add @[...]@ to original builder.
square :: TextBuilder () -> TextBuilder ()
{-# INLINE square #-}
square (TextBuilder b) = TextBuilder (B.square b)

-- | add @<...>@ to original builder.
angle :: TextBuilder () -> TextBuilder ()
{-# INLINE angle #-}
angle (TextBuilder b) = TextBuilder (B.angle b)

-- | add @"..."@ to original builder.
quotes :: TextBuilder () -> TextBuilder ()
{-# INLINE quotes #-}
quotes (TextBuilder b) = TextBuilder (B.quotes b)

-- | add @'...'@ to original builder.
squotes :: TextBuilder () -> TextBuilder ()
{-# INLINE squotes #-}
squotes (TextBuilder b) = TextBuilder (B.squotes b)

-- | write an ASCII @:@
colon ::  TextBuilder ()
{-# INLINE colon #-}
colon = TextBuilder B.colon

-- | write an ASCII @,@
comma ::  TextBuilder ()
{-# INLINE comma #-}
comma = TextBuilder B.comma

-- | Use separator to connect a vector of builders.
intercalateVec :: (V.Vec v a)
               => TextBuilder ()            -- ^ the seperator
               -> (a -> TextBuilder ())     -- ^ value formatter
               -> v a                       -- ^ value list
               ->  TextBuilder ()
{-# INLINE intercalateVec #-}
intercalateVec (TextBuilder s) f = TextBuilder . B.intercalateVec s (getBuilder . f)

-- | Use separator to connect a list of builders.
intercalateList :: TextBuilder ()           -- ^ the seperator
                -> (a -> TextBuilder ())    -- ^ value formatter
                -> [a]                      -- ^ value vector
                -> TextBuilder ()
{-# INLINE intercalateList #-}
intercalateList (TextBuilder s) f = TextBuilder . B.intercalateList s (getBuilder . f)

--------------------------------------------------------------------------------
-- Data types
--
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
--                            deriving anyclass ShowT
--
-- > showT (FooInt 3)
-- > "FooInt 3"
--
--  newtype FooInt = FooInt Int deriving (Generic)
--                            deriving newtype ShowT
--
-- > showT (FooInt 3)
-- > "3"
-- @
--
class ShowT a where
    toTextBuilder :: Int -> a  -> TextBuilder ()
    default toTextBuilder :: (Generic a, GToText (Rep a)) => Int -> a -> TextBuilder ()
    toTextBuilder p = gToTextBuilder p . from

class GToText f where
    gToTextBuilder :: Int -> f a -> TextBuilder ()


class GFieldToText f where
    gFieldToTextBuilder :: B.Builder () -> Int -> f a -> B.Builder ()

instance (GFieldToText a, GFieldToText b) => GFieldToText (a :*: b) where
    {-# INLINE gFieldToTextBuilder #-}
    gFieldToTextBuilder sep p (a :*: b) =
        gFieldToTextBuilder sep p a >> sep >> gFieldToTextBuilder sep p b

instance (GToText f) => GFieldToText (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFieldToTextBuilder #-}
    gFieldToTextBuilder _ p (M1 x) = getBuilder (gToTextBuilder p x)

instance (GToText f, Selector (MetaSel (Just l) u ss ds)) => GFieldToText (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFieldToTextBuilder #-}
    gFieldToTextBuilder _ _ m1@(M1 x) =
        B.stringModifiedUTF8 (selName m1) >> " = " >> getBuilder (gToTextBuilder 0 x)

instance GToText V1 where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder _ = error "Z.Data.TextBuilder: empty data type"

instance (GToText f, GToText g) => GToText (f :+: g) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p (L1 x) = gToTextBuilder p x
    gToTextBuilder p (R1 x) = gToTextBuilder p x

-- | Constructor without payload, convert to String
instance (Constructor c) => GToText (C1 c U1) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder _ m1 =
        TextBuilder . B.stringModifiedUTF8 $ conName m1

-- | Constructor with payloads
instance (GFieldToText (S1 sc f), Constructor c) => GToText (C1 c (S1 sc f)) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p m1@(M1 x) =
        parenWhen (p > 10) . TextBuilder $ do
            B.stringModifiedUTF8 $ conName m1
            B.char8 ' '
            if conIsRecord m1
            then B.curly $ gFieldToTextBuilder (B.char7 ',' >> B.char7 ' ') p x
            else gFieldToTextBuilder (B.char7 ' ') 11 x

instance (GFieldToText (a :*: b), Constructor c) => GToText (C1 c (a :*: b)) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p m1@(M1 x) =
        case conFixity m1 of
            Prefix -> parenWhen (p > 10) . TextBuilder $ do
                B.stringModifiedUTF8 $ conName m1
                B.char8 ' '
                if conIsRecord m1
                then B.curly $ gFieldToTextBuilder (B.char7 ',' >> B.char7 ' ') p x
                else gFieldToTextBuilder (B.char7 ' ') 11 x
            Infix _ p' -> parenWhen (p > p') . TextBuilder $ do
                gFieldToTextBuilder
                    (B.char8 ' ' >> B.stringModifiedUTF8 (conName m1) >> B.char8 ' ') (p'+1) x

instance ShowT a => GToText (K1 i a) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p (K1 x) = toTextBuilder p x

--------------------------------------------------------------------------------
-- Data types
instance GToText f => GToText (D1 c f) where
    {-# INLINE gToTextBuilder #-}
    gToTextBuilder p (M1 x) = gToTextBuilder p x

-- | Directly convert data to 'Text'.
showT :: ShowT a => a -> Text
{-# INLINE showT #-}
showT = buildText .  toTextBuilder 0

-- | Directly convert data to 'B.Builder'.
toBuilder :: ShowT a => a -> B.Builder ()
{-# INLINE toBuilder #-}
toBuilder = getBuilder . toTextBuilder 0

-- | Directly convert data to 'V.Bytes'.
toBytes :: ShowT a => a -> V.Bytes
{-# INLINE toBytes #-}
toBytes = B.buildBytes .  toBuilder

-- | Faster 'show' replacement.
toString :: ShowT a => a -> String
{-# INLINE toString #-}
toString = T.unpack . showT

instance ShowT Bool where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ True = TextBuilder "True"
    toTextBuilder _ _    = TextBuilder "False"

instance ShowT Char where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = TextBuilder . B.string8 . show

instance ShowT Double where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = double;}
instance ShowT Float  where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = float;}

instance ShowT Int     where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Int8    where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Int16   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Int32   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Int64   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Word     where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Word8    where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Word16   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Word32   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}
instance ShowT Word64   where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = int;}

instance ShowT Integer  where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = integer;}
instance ShowT Natural  where {{-# INLINE toTextBuilder #-}; toTextBuilder _ = integer . fromIntegral}
instance ShowT Ordering where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ GT = TextBuilder "GT"
    toTextBuilder _ EQ = TextBuilder "EQ"
    toTextBuilder _ _  = TextBuilder "LT"

instance ShowT () where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ () = TextBuilder "()"

instance ShowT Version where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = stringUTF8 . show

-- | The escaping rules is same with 'Show' instance: we reuse JSON escaping rules here, so it will be faster.
instance ShowT Text where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = TextBuilder . escapeTextJSON

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
--    \'\/\':  \"\\/\"
--    other chars <= 0x1F: "\\u00XX"
-- @
--
escapeTextJSON :: T.Text -> B.Builder ()
{-# INLINE escapeTextJSON #-}
escapeTextJSON (T.Text (V.PrimVector ba@(PrimArray ba#) s l)) = do
    let siz = escape_json_string_length ba# s l
    B.ensureN siz
    B.Builder (\ k (B.Buffer mba@(MutablePrimArray mba#) i) -> do
        if siz == l+2   -- no need to escape
        then do
            writePrimArray mba i DOUBLE_QUOTE
            copyPrimArray mba (i+1) ba s l
            writePrimArray mba (i+1+l) DOUBLE_QUOTE
        else void (escape_json_string ba# s l (unsafeCoerce# mba#) i)
        k () (B.Buffer mba (i+siz)))

foreign import ccall unsafe escape_json_string_length
    :: ByteArray# -> Int -> Int -> Int

foreign import ccall unsafe escape_json_string
    :: ByteArray# -> Int -> Int -> MutableByteArray# RealWorld -> Int -> IO Int

instance ShowT Sci.Scientific where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = scientific

instance ShowT a => ShowT [a] where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateList comma (toTextBuilder 0)

instance ShowT a => ShowT (A.Array a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateVec comma (toTextBuilder 0)

instance ShowT a => ShowT (A.SmallArray a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateVec comma (toTextBuilder 0)

instance (A.PrimUnlifted a, ShowT a) => ShowT (A.UnliftedArray a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateVec comma (toTextBuilder 0)

instance (Prim a, ShowT a) => ShowT (A.PrimArray a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateVec comma (toTextBuilder 0)

instance ShowT a => ShowT (V.Vector a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateVec comma (toTextBuilder 0)

instance (Prim a, ShowT a) => ShowT (V.PrimVector a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = square . intercalateVec comma (toTextBuilder 0)

instance (ShowT a, ShowT b) => ShowT (a, b) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b

instance (ShowT a, ShowT b, ShowT c) => ShowT (a, b, c) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c

instance (ShowT a, ShowT b, ShowT c, ShowT d) => ShowT (a, b, c, d) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c, d) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c
                     >> comma >> toTextBuilder 0 d

instance (ShowT a, ShowT b, ShowT c, ShowT d, ShowT e) => ShowT (a, b, c, d, e) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c, d, e) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c
                     >> comma >> toTextBuilder 0 d
                     >> comma >> toTextBuilder 0 e

instance (ShowT a, ShowT b, ShowT c, ShowT d, ShowT e, ShowT f) => ShowT (a, b, c, d, e, f) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c, d, e, f) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c
                     >> comma >> toTextBuilder 0 d
                     >> comma >> toTextBuilder 0 e
                     >> comma >> toTextBuilder 0 f

instance (ShowT a, ShowT b, ShowT c, ShowT d, ShowT e, ShowT f, ShowT g) => ShowT (a, b, c, d, e, f, g) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (a, b, c, d, e, f, g) = paren $  toTextBuilder 0 a
                     >> comma >> toTextBuilder 0 b
                     >> comma >> toTextBuilder 0 c
                     >> comma >> toTextBuilder 0 d
                     >> comma >> toTextBuilder 0 e
                     >> comma >> toTextBuilder 0 f
                     >> comma >> toTextBuilder 0 g

instance ShowT a => ShowT (Maybe a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder p (Just x) = parenWhen (p > 10) $ do TextBuilder "Just "
                                                       toTextBuilder 11 x
    toTextBuilder _ _        = TextBuilder "Nothing"

instance (ShowT a, ShowT b) => ShowT (Either a b) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder p (Left x) = parenWhen (p > 10) $ do TextBuilder "Left "
                                                       toTextBuilder 11 x
    toTextBuilder p (Right x) = parenWhen (p > 10) $ do TextBuilder "Right "
                                                        toTextBuilder 11 x

instance (ShowT a, Integral a) => ShowT (Ratio a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder p r = parenWhen (p > 10) $ do toTextBuilder 8 (numerator r)
                                                TextBuilder " % "
                                                toTextBuilder 8 (denominator r)

instance HasResolution a => ShowT (Fixed a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = TextBuilder . B.string8 .  show

instance ShowT CallStack where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ = TextBuilder . B.string8 .  show

deriving newtype instance ShowT CChar
deriving newtype instance ShowT CSChar
deriving newtype instance ShowT CUChar
deriving newtype instance ShowT CShort
deriving newtype instance ShowT CUShort
deriving newtype instance ShowT CInt
deriving newtype instance ShowT CUInt
deriving newtype instance ShowT CLong
deriving newtype instance ShowT CULong
deriving newtype instance ShowT CPtrdiff
deriving newtype instance ShowT CSize
deriving newtype instance ShowT CWchar
deriving newtype instance ShowT CSigAtomic
deriving newtype instance ShowT CLLong
deriving newtype instance ShowT CULLong
deriving newtype instance ShowT CBool
deriving newtype instance ShowT CIntPtr
deriving newtype instance ShowT CUIntPtr
deriving newtype instance ShowT CIntMax
deriving newtype instance ShowT CUIntMax
deriving newtype instance ShowT CClock
deriving newtype instance ShowT CTime
deriving newtype instance ShowT CUSeconds
deriving newtype instance ShowT CSUSeconds
deriving newtype instance ShowT CFloat
deriving newtype instance ShowT CDouble

instance ShowT (Ptr a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (Ptr a) =
        "0x" >> hex (W# (int2Word#(addr2Int# a)))
instance ShowT (ForeignPtr a) where
    {-# INLINE toTextBuilder #-}
    toTextBuilder _ (ForeignPtr a _) =
        "0x" >> hex (W# (int2Word#(addr2Int# a)))

deriving anyclass instance ShowT ExitCode

deriving anyclass instance ShowT a => ShowT (Semigroup.Min a)
deriving anyclass instance ShowT a => ShowT (Semigroup.Max a)
deriving anyclass instance ShowT a => ShowT (Semigroup.First a)
deriving anyclass instance ShowT a => ShowT (Semigroup.Last a)
deriving anyclass instance ShowT a => ShowT (Semigroup.WrappedMonoid a)
deriving anyclass instance ShowT a => ShowT (Semigroup.Dual a)
deriving anyclass instance ShowT a => ShowT (Monoid.First a)
deriving anyclass instance ShowT a => ShowT (Monoid.Last a)
deriving anyclass instance ShowT a => ShowT (NonEmpty a)
deriving anyclass instance ShowT a => ShowT (Identity a)
deriving anyclass instance ShowT a => ShowT (Const a b)
deriving anyclass instance ShowT (Proxy a)
deriving anyclass instance ShowT b => ShowT (Tagged a b)
deriving anyclass instance ShowT (f (g a)) => ShowT (Compose f g a)
deriving anyclass instance (ShowT (f a), ShowT (g a)) => ShowT (Product f g a)
deriving anyclass instance (ShowT (f a), ShowT (g a), ShowT a) => ShowT (Sum f g a)
