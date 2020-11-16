{-|
Module      : Z.Data.Text.Builder
Description : UTF8 compatible builders.
Copyright   : (c) Dong Han, 2017-2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module re-exports some UTF8 compatible textual builders from 'Z.Data.Builder'.

We also provide a faster alternative to 'Show' class, i.e. 'ShowT', which can be deriving using 'Generic'.
For example to use 'ShowT' class:

@
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}

import qualified Z.Data.Text.ShowT as T

data Foo = Bar Bytes | Qux Text Int deriving Generic
                                    deriving anyclass T.ShowT

@

-}

module Z.Data.Text.ShowT
  ( -- * ShowT class
    ShowT(..), toText, toString, toUTF8Builder, toUTF8Bytes
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
  , B.scientificWith
  -- * Helpers
  , B.paren, B.curly, B.square, B.angle, B.quotes, B.squotes
  , B.colon, B.comma, B.intercalateVec, B.intercalateList
  , parenWhen
  ) where

import           Control.Monad
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
--                            deriving anyclass ShowT
--
-- > toText (FooInt 3)
-- > "FooInt 3"
--
--  newtype FooInt = FooInt Int deriving (Generic)
--                            deriving newtype ShowT
--
-- > toText (FooInt 3)
-- > "3"
-- @
--
class ShowT a where
    -- | Convert data to 'B.Builder' with precendence.
    --
    -- You should return a 'B.Builder' writing in UTF8 encoding only, i.e.
    --
    -- @Z.Data.Text.validateMaybe (Z.Data.Builder.buildBytes (toUTF8BuilderP p a)) /= Nothing@
    toUTF8BuilderP :: Int -> a  -> B.Builder ()

    default toUTF8BuilderP :: (Generic a, GToText (Rep a)) => Int -> a -> B.Builder ()
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP p = gToUTF8BuilderP p . from

-- | Convert data to 'B.Builder'.
toUTF8Builder :: ShowT a => a  -> B.Builder ()
{-# INLINE toUTF8Builder #-}
toUTF8Builder = toUTF8BuilderP 0

-- | Convert data to 'V.Bytes' in UTF8 encoding.
toUTF8Bytes :: ShowT a => a -> V.Bytes
{-# INLINE toUTF8Bytes #-}
toUTF8Bytes = B.buildBytes . toUTF8BuilderP 0

-- | Convert data to 'Text'.
toText :: ShowT a => a -> Text
{-# INLINE toText #-}
toText = Text . toUTF8Bytes

-- | Convert data to 'String', faster 'show' replacement.
toString :: ShowT a => a -> String
{-# INLINE toString #-}
toString = T.unpack . toText

class GToText f where
    gToUTF8BuilderP :: Int -> f a -> B.Builder ()

class GFieldToText f where
    gFieldToUTF8BuilderP :: B.Builder () -> Int -> f a -> B.Builder ()

instance (GFieldToText a, GFieldToText b) => GFieldToText (a :*: b) where
    {-# INLINE gFieldToUTF8BuilderP #-}
    gFieldToUTF8BuilderP sep p (a :*: b) =
        gFieldToUTF8BuilderP sep p a >> sep >> gFieldToUTF8BuilderP sep p b

instance (GToText f) => GFieldToText (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFieldToUTF8BuilderP #-}
    gFieldToUTF8BuilderP _ p (M1 x) = gToUTF8BuilderP p x

instance (GToText f, Selector (MetaSel (Just l) u ss ds)) => GFieldToText (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFieldToUTF8BuilderP #-}
    gFieldToUTF8BuilderP _ _ m1@(M1 x) =
        B.stringModifiedUTF8 (selName m1) >> " = " >> gToUTF8BuilderP 0 x

instance GToText V1 where
    {-# INLINE gToUTF8BuilderP #-}
    gToUTF8BuilderP _ = error "Z.Data.Text.ShowT: empty data type"

instance (GToText f, GToText g) => GToText (f :+: g) where
    {-# INLINE gToUTF8BuilderP #-}
    gToUTF8BuilderP p (L1 x) = gToUTF8BuilderP p x
    gToUTF8BuilderP p (R1 x) = gToUTF8BuilderP p x

-- | Constructor without payload, convert to String
instance (Constructor c) => GToText (C1 c U1) where
    {-# INLINE gToUTF8BuilderP #-}
    gToUTF8BuilderP _ m1 = B.stringModifiedUTF8 $ conName m1

-- | Constructor with payloads
instance (GFieldToText (S1 sc f), Constructor c) => GToText (C1 c (S1 sc f)) where
    {-# INLINE gToUTF8BuilderP #-}
    gToUTF8BuilderP p m1@(M1 x) =
        parenWhen (p > 10) $ do
            B.stringModifiedUTF8 $ conName m1
            B.char8 ' '
            if conIsRecord m1
            then B.curly $ gFieldToUTF8BuilderP (B.char7 ',' >> B.char7 ' ') p x
            else gFieldToUTF8BuilderP (B.char7 ' ') 11 x

instance (GFieldToText (a :*: b), Constructor c) => GToText (C1 c (a :*: b)) where
    {-# INLINE gToUTF8BuilderP #-}
    gToUTF8BuilderP p m1@(M1 x) =
        case conFixity m1 of
            Prefix -> parenWhen (p > 10) $ do
                B.stringModifiedUTF8 $ conName m1
                B.char8 ' '
                if conIsRecord m1
                then B.curly $ gFieldToUTF8BuilderP (B.char7 ',' >> B.char7 ' ') p x
                else gFieldToUTF8BuilderP (B.char7 ' ') 11 x
            Infix _ p' -> parenWhen (p > p') $ do
                gFieldToUTF8BuilderP
                    (B.char8 ' ' >> B.stringModifiedUTF8 (conName m1) >> B.char8 ' ') (p'+1) x

instance ShowT a => GToText (K1 i a) where
    {-# INLINE gToUTF8BuilderP #-}
    gToUTF8BuilderP p (K1 x) = toUTF8BuilderP p x

-- | Add "(..)" around builders when condition is met, otherwise add nothing.
--
-- This is useful when defining 'ShowT' instances.
parenWhen :: Bool -> B.Builder () -> B.Builder ()
{-# INLINE parenWhen #-}
parenWhen True b = B.paren b
parenWhen _    b = b

--------------------------------------------------------------------------------
-- Data types
instance GToText f => GToText (D1 c f) where
    {-# INLINE gToUTF8BuilderP #-}
    gToUTF8BuilderP p (M1 x) = gToUTF8BuilderP p x

instance ShowT Bool where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ True = "True"
    toUTF8BuilderP _ _    = "False"


instance ShowT Char where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.string8 . show

instance ShowT Double where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.double;}
instance ShowT Float  where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.float;}

instance ShowT Int     where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Int8    where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Int16   where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Int32   where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Int64   where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Word    where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Word8   where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Word16  where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Word32  where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}
instance ShowT Word64  where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.int;}

instance ShowT Integer  where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.integer;}
instance ShowT Natural  where {{-# INLINE toUTF8BuilderP #-}; toUTF8BuilderP _ = B.integer . fromIntegral}
instance ShowT Ordering where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ GT = "GT"
    toUTF8BuilderP _ EQ = "EQ"
    toUTF8BuilderP _ _  = "LT"

instance ShowT () where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ () = "()"

instance ShowT Version where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.stringUTF8 . show

-- | The escaping rules is same with 'Show' instance: we reuse JSON escaping rules here, so it will be faster.
instance ShowT Text where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = escapeTextJSON

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
    let !siz = escape_json_string_length ba# s l
    B.writeN siz (\ mba@(MutablePrimArray mba#) i -> do
        if siz == l+2   -- no need to escape
        then do
            writePrimArray mba i DOUBLE_QUOTE
            copyPrimArray mba (i+1) ba s l
            writePrimArray mba (i+1+l) DOUBLE_QUOTE
        else void (escape_json_string ba# s l (unsafeCoerce# mba#) i))

foreign import ccall unsafe escape_json_string_length
    :: ByteArray# -> Int -> Int -> Int

foreign import ccall unsafe escape_json_string
    :: ByteArray# -> Int -> Int -> MutableByteArray# RealWorld -> Int -> IO Int

instance ShowT Sci.Scientific where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.scientific

instance ShowT a => ShowT [a] where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.square . B.intercalateList B.comma (toUTF8BuilderP 0)

instance ShowT a => ShowT (A.Array a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.square . B.intercalateVec B.comma (toUTF8BuilderP 0)

instance ShowT a => ShowT (A.SmallArray a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.square . B.intercalateVec B.comma (toUTF8BuilderP 0)

instance (A.PrimUnlifted a, ShowT a) => ShowT (A.UnliftedArray a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.square . B.intercalateVec B.comma (toUTF8BuilderP 0)

instance (Prim a, ShowT a) => ShowT (A.PrimArray a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.square . B.intercalateVec B.comma (toUTF8BuilderP 0)

instance ShowT a => ShowT (V.Vector a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.square . B.intercalateVec B.comma (toUTF8BuilderP 0)

instance (Prim a, ShowT a) => ShowT (V.PrimVector a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.square . B.intercalateVec B.comma (toUTF8BuilderP 0)

instance (ShowT a, ShowT b) => ShowT (a, b) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (a, b) = B.paren $  toUTF8BuilderP 0 a
                     >> B.comma >> toUTF8BuilderP 0 b

instance (ShowT a, ShowT b, ShowT c) => ShowT (a, b, c) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (a, b, c) = B.paren $  toUTF8BuilderP 0 a
                     >> B.comma >> toUTF8BuilderP 0 b
                     >> B.comma >> toUTF8BuilderP 0 c

instance (ShowT a, ShowT b, ShowT c, ShowT d) => ShowT (a, b, c, d) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (a, b, c, d) = B.paren $  toUTF8BuilderP 0 a
                     >> B.comma >> toUTF8BuilderP 0 b
                     >> B.comma >> toUTF8BuilderP 0 c
                     >> B.comma >> toUTF8BuilderP 0 d

instance (ShowT a, ShowT b, ShowT c, ShowT d, ShowT e) => ShowT (a, b, c, d, e) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (a, b, c, d, e) = B.paren $  toUTF8BuilderP 0 a
                     >> B.comma >> toUTF8BuilderP 0 b
                     >> B.comma >> toUTF8BuilderP 0 c
                     >> B.comma >> toUTF8BuilderP 0 d
                     >> B.comma >> toUTF8BuilderP 0 e

instance (ShowT a, ShowT b, ShowT c, ShowT d, ShowT e, ShowT f) => ShowT (a, b, c, d, e, f) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (a, b, c, d, e, f) = B.paren $  toUTF8BuilderP 0 a
                     >> B.comma >> toUTF8BuilderP 0 b
                     >> B.comma >> toUTF8BuilderP 0 c
                     >> B.comma >> toUTF8BuilderP 0 d
                     >> B.comma >> toUTF8BuilderP 0 e
                     >> B.comma >> toUTF8BuilderP 0 f

instance (ShowT a, ShowT b, ShowT c, ShowT d, ShowT e, ShowT f, ShowT g) => ShowT (a, b, c, d, e, f, g) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (a, b, c, d, e, f, g) = B.paren $  toUTF8BuilderP 0 a
                     >> B.comma >> toUTF8BuilderP 0 b
                     >> B.comma >> toUTF8BuilderP 0 c
                     >> B.comma >> toUTF8BuilderP 0 d
                     >> B.comma >> toUTF8BuilderP 0 e
                     >> B.comma >> toUTF8BuilderP 0 f
                     >> B.comma >> toUTF8BuilderP 0 g

instance ShowT a => ShowT (Maybe a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP p (Just x) = parenWhen (p > 10) $ "Just " >> toUTF8BuilderP 11 x
    toUTF8BuilderP _ _        = "Nothing"

instance (ShowT a, ShowT b) => ShowT (Either a b) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP p (Left x) = parenWhen (p > 10) $ "Left " >> toUTF8BuilderP 11 x
    toUTF8BuilderP p (Right x) = parenWhen (p > 10) $ "Right " >> toUTF8BuilderP 11 x

instance (ShowT a, Integral a) => ShowT (Ratio a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP p r = parenWhen (p > 10) $ do
        toUTF8BuilderP 8 (numerator r)
        " % "
        toUTF8BuilderP 8 (denominator r)

instance HasResolution a => ShowT (Fixed a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.string8 .  show

instance ShowT CallStack where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ = B.string8 .  show

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
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (Ptr a) =
        "0x" >> B.hex (W# (int2Word#(addr2Int# a)))
instance ShowT (ForeignPtr a) where
    {-# INLINE toUTF8BuilderP #-}
    toUTF8BuilderP _ (ForeignPtr a _) =
        "0x" >> B.hex (W# (int2Word#(addr2Int# a)))

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
