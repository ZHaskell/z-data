{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Z.Data.JSON.BaseSpec where

import qualified Data.List                      as L
import           Data.Word
import           Data.Int
import           Data.Either
import           GHC.Generics
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.Map.Strict                as M
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as Set
import qualified Data.Tree                      as Tree
import qualified Z.Data.Text                    as T
import qualified Z.Data.Vector                  as V
import qualified Z.Data.Builder                 as B
import           Data.Time                      (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import           Data.Time.Calendar             (CalendarDiffDays (..), DayOfWeek (..))
import           Data.Time.LocalTime            (CalendarDiffTime (..))
import           Data.Time.Clock.System         (SystemTime (..))
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Instances
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Z.Data.JSON                    as JSON
import           Z.Data.JSON (JSON, Value(..))


data T a
    = Nullary
    | Unary Int
    | Product T.Text (Maybe Char) a
    | Record { testOne   :: Double
             , testTwo   :: Maybe Bool
             , testThree :: Maybe a
             }

    | RecordII { testFour   :: Double }
    | List [a]
   deriving (Show, Eq, Generic, JSON)

mid :: JSON a => a -> a
mid = fromRight (error "decode failed") . JSON.decode' . JSON.encode

intMid :: Int64 -> Int64
intMid = mid

encodeText' :: JSON a => a -> T.Text
encodeText' = JSON.encodeText . JSON.toValue

spec :: Spec
spec = do
    describe "JSON Base instances roundtrip" $ do

        it "Nullary constructor are encoded as text" $
            JSON.encodeText (Nullary :: T Integer) === "\"Nullary\""

        it "Unary constructor are encoded as single field object" $
            JSON.encodeText (Unary 123456 :: T Integer) === "{\"Unary\":123456}"

        it "Product are encoded as array" $
            JSON.encodeText (Product "ABC" (Just 'x') (123456::Integer)) ===
                "{\"Product\":[\"ABC\",\"x\",123456]}"

        it "Record are encoded as key values" $
            JSON.encodeText (Record 0.123456 Nothing (Just (123456::Integer))) ===
                "{\"Record\":{\
                    \\"testOne\":0.123456,\
                    \\"testTwo\":null,\
                    \\"testThree\":123456}}"

        it "Record are encoded as key values(single field)" $
            JSON.encodeText (RecordII 0.123456 :: T Integer) ===
                "{\"RecordII\":{\"testFour\":0.123456}}"

        it "List are encode as array" $
            JSON.encodeText (List [Nullary
                , Unary 123456
                , (Product "ABC" (Just 'x') (123456::Integer))
                , (Record 0.123456 Nothing (Just (123456::Integer)))]) ===
                    "{\"List\":[\"Nullary\",\
                    \{\"Unary\":123456},\
                    \{\"Product\":[\"ABC\",\"x\",123456]},\
                    \{\"Record\":{\
                    \\"testOne\":0.123456,\
                    \\"testTwo\":null,\
                    \\"testThree\":123456}}]}"

        it "control characters are escaped" $
            JSON.encodeText (T.pack $ map toEnum [0..0x1F]) ===
                "\"\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\b\\t\\n\\u000b\\f\\r\\u000e\\u000f\
                \\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\
                \\\u001a\\u001b\\u001c\\u001d\\u001e\\u001f\""

        -- tests from MessagePack suit
        it "int"    $ property $ \(a :: Int   ) -> a `shouldBe` mid a
        it "int8"   $ property $ \(a :: Int8  ) -> a `shouldBe` mid a
        it "int16"  $ property $ \(a :: Int16 ) -> a `shouldBe` mid a
        it "int32"  $ property $ \(a :: Int32 ) -> a `shouldBe` mid a
        it "int64"  $ property $ \(a :: Int64 ) -> a `shouldBe` mid a
        it "word"   $ property $ \(a :: Word  ) -> a `shouldBe` mid a
        it "word8"  $ property $ \(a :: Word8 ) -> a `shouldBe` mid a
        it "word16" $ property $ \(a :: Word16) -> a `shouldBe` mid a
        it "word32" $ property $ \(a :: Word32) -> a `shouldBe` mid a
        it "word64" $ property $ \(a :: Word64) -> a `shouldBe` mid a

        it "()"                                  $ property $ \(a :: ())                                   -> a `shouldBe` mid a
        it "bool"                                $ property $ \(a :: Bool)                                 -> a `shouldBe` mid a
        it "float"                               $ property $ \(a :: Float)                                -> a `shouldBe` mid a
        it "double"                              $ property $ \(a :: Double)                               -> a `shouldBe` mid a
        it "string"                              $ property $ \(a :: String)                               -> a `shouldBe` mid a
        it "bytes"                               $ property $ \(a :: V.Bytes)                              -> a `shouldBe` mid a
        it "primvector"                          $ property $ \(a :: V.PrimVector Int)                     -> a `shouldBe` mid a
        it "vector"                              $ property $ \(a :: V.Vector [Integer])                   -> a `shouldBe` mid a
        it "maybe int"                           $ property $ \(a :: (Maybe Int))                          -> a `shouldBe` mid a
        it "[int]"                               $ property $ \(a :: [Int])                                -> a `shouldBe` mid a
        it "[string]"                            $ property $ \(a :: [String])                             -> a `shouldBe` mid a
        it "(int, int)"                          $ property $ \(a :: (Int, Int))                           -> a `shouldBe` mid a
        it "(int, int, int)"                     $ property $ \(a :: (Int, Int, Int))                      -> a `shouldBe` mid a
        it "(int, int, int, int)"                $ property $ \(a :: (Int, Int, Int, Int))                 -> a `shouldBe` mid a
        it "(int, int, int, int, int)"           $ property $ \(a :: (Int, Int, Int, Int, Int))            -> a `shouldBe` mid a
        it "(int, int, int, int, int, int)"      $ property $ \(a :: (Int, Int, Int, Int, Int, Int))       -> a `shouldBe` mid a
        it "(int, int, int, int, int, int, int)" $ property $ \(a :: (Int, Int, Int, Int, Int, Int, Int))  -> a `shouldBe` mid a
        it "[(int, double)]"                     $ property $ \(a :: [(Int, Double)])                      -> a `shouldBe` mid a
        it "[(string, string)]"                  $ property $ \(a :: [(String, String)])                   -> a `shouldBe` mid a
        it "HashMap Text Int"                    $ property $ \(a :: HM.HashMap T.Text Int)                -> a `shouldBe` mid a
        it "HashSet Text"                        $ property $ \(a :: HS.HashSet T.Text)                    -> a `shouldBe` mid a
        it "Map Text Int"                        $ property $ \(a :: M.Map T.Text Int)                     -> a `shouldBe` mid a
        it "IntMap Int"                          $ property $ \(a :: IM.IntMap Int)                        -> a `shouldBe` mid a
        it "Set Int"                             $ property $ \(a :: Set.Set Int)                          -> a `shouldBe` mid a
        it "IntSet"                              $ property $ \(a :: IS.IntSet)                            -> a `shouldBe` mid a
        it "Seq Int"                             $ property $ \(a :: Seq.Seq Int)                          -> a `shouldBe` mid a
        it "Tree Int"                            $ property $ \(a :: Tree.Tree Int)                        -> a `shouldBe` mid a
        it "maybe int"                           $ property $ \(a :: Maybe Int)                            -> a `shouldBe` mid a
        it "maybe nil"                           $ property $ \(a :: Maybe ())                             -> a `shouldBe` mid a
        it "maybe bool"                          $ property $ \(a :: Maybe Bool)                           -> a `shouldBe` mid a
        it "maybe double"                        $ property $ \(a :: Maybe Double)                         -> a `shouldBe` mid a
        it "maybe string"                        $ property $ \(a :: Maybe String)                         -> a `shouldBe` mid a
        it "maybe bytes"                         $ property $ \ (a :: Maybe V.Bytes)                       -> a `shouldBe` mid a
        it "maybe [int]"                         $ property $ \(a :: Maybe [Int])                          -> a `shouldBe` mid a
        it "maybe [string]"                      $ property $ \(a :: Maybe [String])                       -> a `shouldBe` mid a
        it "maybe (int, int)"                    $ property $ \(a :: Maybe (Int, Int))                     -> a `shouldBe` mid a
        it "maybe (int, int, int)"               $ property $ \(a :: Maybe (Int, Int, Int))                -> a `shouldBe` mid a
        it "maybe (int, int, int, int)"          $ property $ \(a :: Maybe (Int, Int, Int, Int))           -> a `shouldBe` mid a
        it "maybe (int, int, int, int, int)"     $ property $ \(a :: Maybe (Int, Int, Int, Int, Int))      -> a `shouldBe` mid a
        it "maybe [(int, double)]"               $ property $ \(a :: Maybe [(Int, Double)])                -> a `shouldBe` mid a
        it "maybe [(string, string)]"            $ property $ \(a :: Maybe [(String, String)])             -> a `shouldBe` mid a
        it "either int float"                    $ property $ \(a :: Either Int Float)                     -> a `shouldBe` mid a
        it "Day"                                 $ property $ \(a :: Day)                                  -> a `shouldBe` mid a
        it "DiffTime"                            $ property $ \(a :: DiffTime)                             -> a `shouldBe` mid a
        it "LocalTime"                           $ property $ \(a :: LocalTime)                            -> a `shouldBe` mid a
        it "NominalDiffTime"                     $ property $ \(a :: NominalDiffTime)                      -> a `shouldBe` mid a
        it "TimeOfDay"                           $ property $ \(a :: TimeOfDay)                            -> a `shouldBe` mid a
        it "UTCTime"                             $ property $ \(a :: UTCTime)                              -> a `shouldBe` mid a
        it "SystemTime"                          $ property $ \(a :: SystemTime)                           -> a `shouldBe` mid a
        it "CalendarDiffDays"                    $ property $ \(a :: CalendarDiffDays)                     -> a `shouldBe` mid a
        it "DayOfWeek"                           $ property $ \(a :: DayOfWeek)                            -> a `shouldBe` mid a
        it "CalendarDiffTime"                    $ property $ \(a :: CalendarDiffTime)                     -> a `shouldBe` mid a
        it "arbitrary message"                   $ property $ \(a :: Value)                                -> a `shouldBe` mid a

    describe "JSON Base instances encodeJSON == encodeJSON . toValue" $ do

        it "Nullary constructor are encoded as text" $
            JSON.encodeText (Nullary :: T Integer) ===
                encodeText' (Nullary :: T Integer)

        it "Unary constructor are encoded as single field object" $
            JSON.encodeText (Unary 123456 :: T Integer) ===
                encodeText' (Unary 123456 :: T Integer)

        it "Product are encoded as array" $
            JSON.encodeText (Product "ABC" (Just 'x') (123456::Integer)) ===
                encodeText' (Product "ABC" (Just 'x') (123456::Integer))

        it "Record are encoded as key values" $
            JSON.encodeText (Record 0.123456 Nothing (Just (123456::Integer))) ===
                encodeText' (Record 0.123456 Nothing (Just (123456::Integer)))

        it "Record are encoded as key values(single field)" $
            JSON.encodeText (RecordII 0.123456 :: T Integer) ===
                encodeText' (RecordII 0.123456 :: T Integer)

        it "List are encode as array" $
            JSON.encodeText (List [Nullary
                , Unary 123456
                , (Product "ABC" (Just 'x') (123456::Integer))
                , (Record 0.123456 Nothing (Just (123456::Integer)))]) ===
                encodeText' (List [Nullary
                    , Unary 123456
                    , (Product "ABC" (Just 'x') (123456::Integer))
                    , (Record 0.123456 Nothing (Just (123456::Integer)))])

        it "control characters are escaped" $
            JSON.encodeText (T.pack $ map toEnum [0..0x1F]) ===
                encodeText' (T.pack $ map toEnum [0..0x1F])

        -- tests from MessagePack suit
        it "int"    $ property $ \(a :: Int   ) -> encodeText' a === JSON.encodeText a
        it "int8"   $ property $ \(a :: Int8  ) -> encodeText' a === JSON.encodeText a
        it "int16"  $ property $ \(a :: Int16 ) -> encodeText' a === JSON.encodeText a
        it "int32"  $ property $ \(a :: Int32 ) -> encodeText' a === JSON.encodeText a
        it "int64"  $ property $ \(a :: Int64 ) -> encodeText' a === JSON.encodeText a
        it "word"   $ property $ \(a :: Word  ) -> encodeText' a === JSON.encodeText a
        it "word8"  $ property $ \(a :: Word8 ) -> encodeText' a === JSON.encodeText a
        it "word16" $ property $ \(a :: Word16) -> encodeText' a === JSON.encodeText a
        it "word32" $ property $ \(a :: Word32) -> encodeText' a === JSON.encodeText a
        it "word64" $ property $ \(a :: Word64) -> encodeText' a === JSON.encodeText a

        it "()"                                  $ property $ \(a :: ())                                   -> encodeText' a === JSON.encodeText a
        it "bool"                                $ property $ \(a :: Bool)                                 -> encodeText' a === JSON.encodeText a
        -- | 0.0 /== 0
        -- it "float"                               $ property $ \(a :: Float)                                -> encodeText' a === JSON.encodeText a
        -- it "double"                              $ property $ \(a :: Double)                               -> encodeText' a === JSON.encodeText a
        it "string"                              $ property $ \(a :: String)                               -> encodeText' a === JSON.encodeText a
        it "bytes"                               $ property $ \(a :: V.Bytes)                              -> encodeText' a === JSON.encodeText a
        it "primvector"                          $ property $ \(a :: V.PrimVector Int)                     -> encodeText' a === JSON.encodeText a
        it "vector"                              $ property $ \(a :: V.Vector [Integer])                   -> encodeText' a === JSON.encodeText a
        it "maybe int"                           $ property $ \(a :: (Maybe Int))                          -> encodeText' a === JSON.encodeText a
        it "[int]"                               $ property $ \(a :: [Int])                                -> encodeText' a === JSON.encodeText a
        it "[string]"                            $ property $ \(a :: [String])                             -> encodeText' a === JSON.encodeText a
        it "(int, int)"                          $ property $ \(a :: (Int, Int))                           -> encodeText' a === JSON.encodeText a
        it "(int, int, int)"                     $ property $ \(a :: (Int, Int, Int))                      -> encodeText' a === JSON.encodeText a
        it "(int, int, int, int)"                $ property $ \(a :: (Int, Int, Int, Int))                 -> encodeText' a === JSON.encodeText a
        it "(int, int, int, int, int)"           $ property $ \(a :: (Int, Int, Int, Int, Int))            -> encodeText' a === JSON.encodeText a
        it "(int, int, int, int, int, int)"      $ property $ \(a :: (Int, Int, Int, Int, Int, Int))       -> encodeText' a === JSON.encodeText a
        it "(int, int, int, int, int, int, int)" $ property $ \(a :: (Int, Int, Int, Int, Int, Int, Int))  -> encodeText' a === JSON.encodeText a
        it "[(int, double)]"                     $ property $ \(a :: [(Int, Double)])                      -> encodeText' a === JSON.encodeText a
        it "[(string, string)]"                  $ property $ \(a :: [(String, String)])                   -> encodeText' a === JSON.encodeText a
        it "HashMap Text Int"                    $ property $ \(a :: HM.HashMap T.Text Int)                -> encodeText' a === JSON.encodeText a
        it "HashSet Text"                        $ property $ \(a :: HS.HashSet T.Text)                    -> encodeText' a === JSON.encodeText a
        it "Map Text Int"                        $ property $ \(a :: M.Map T.Text Int)                     -> encodeText' a === JSON.encodeText a
        it "IntMap Int"                          $ property $ \(a :: IM.IntMap Int)                        -> encodeText' a === JSON.encodeText a
        it "Set Int"                             $ property $ \(a :: Set.Set Int)                          -> encodeText' a === JSON.encodeText a
        it "IntSet"                              $ property $ \(a :: IS.IntSet)                            -> encodeText' a === JSON.encodeText a
        it "Seq Int"                             $ property $ \(a :: Seq.Seq Int)                          -> encodeText' a === JSON.encodeText a
        it "Tree Int"                            $ property $ \(a :: Tree.Tree Int)                        -> encodeText' a === JSON.encodeText a
        it "maybe int"                           $ property $ \(a :: Maybe Int)                            -> encodeText' a === JSON.encodeText a
        it "maybe nil"                           $ property $ \(a :: Maybe ())                             -> encodeText' a === JSON.encodeText a
        it "maybe bool"                          $ property $ \(a :: Maybe Bool)                           -> encodeText' a === JSON.encodeText a
        -- | 0.0 /== 0
        -- it "maybe double"                        $ property $ \(a :: Maybe Double)                         -> encodeText' a === JSON.encodeText a
        it "maybe string"                        $ property $ \(a :: Maybe String)                         -> encodeText' a === JSON.encodeText a
        it "maybe bytes"                         $ property $ \ (a :: Maybe V.Bytes)                       -> encodeText' a === JSON.encodeText a
        it "maybe [int]"                         $ property $ \(a :: Maybe [Int])                          -> encodeText' a === JSON.encodeText a
        it "maybe [string]"                      $ property $ \(a :: Maybe [String])                       -> encodeText' a === JSON.encodeText a
        it "maybe (int, int)"                    $ property $ \(a :: Maybe (Int, Int))                     -> encodeText' a === JSON.encodeText a
        it "maybe (int, int, int)"               $ property $ \(a :: Maybe (Int, Int, Int))                -> encodeText' a === JSON.encodeText a
        it "maybe (int, int, int, int)"          $ property $ \(a :: Maybe (Int, Int, Int, Int))           -> encodeText' a === JSON.encodeText a
        it "maybe (int, int, int, int, int)"     $ property $ \(a :: Maybe (Int, Int, Int, Int, Int))      -> encodeText' a === JSON.encodeText a
        it "maybe [(int, double)]"               $ property $ \(a :: Maybe [(Int, Double)])                -> encodeText' a === JSON.encodeText a
        it "maybe [(string, string)]"            $ property $ \(a :: Maybe [(String, String)])             -> encodeText' a === JSON.encodeText a
        -- | 0.0 /== 0
        -- it "either int float"                    $ property $ \(a :: Either Int Float)                     -> encodeText' a === JSON.encodeText a
        it "Day"                                 $ property $ \(a :: Day)                                  -> encodeText' a === JSON.encodeText a
        it "DiffTime"                            $ property $ \(a :: DiffTime)                             -> encodeText' a === JSON.encodeText a
        it "LocalTime"                           $ property $ \(a :: LocalTime)                            -> encodeText' a === JSON.encodeText a
        it "NominalDiffTime"                     $ property $ \(a :: NominalDiffTime)                      -> encodeText' a === JSON.encodeText a
        it "TimeOfDay"                           $ property $ \(a :: TimeOfDay)                            -> encodeText' a === JSON.encodeText a
        it "UTCTime"                             $ property $ \(a :: UTCTime)                              -> encodeText' a === JSON.encodeText a
        it "SystemTime"                          $ property $ \(a :: SystemTime)                           -> encodeText' a === JSON.encodeText a
        it "CalendarDiffDays"                    $ property $ \(a :: CalendarDiffDays)                     -> encodeText' a === JSON.encodeText a
        it "DayOfWeek"                           $ property $ \(a :: DayOfWeek)                            -> encodeText' a === JSON.encodeText a
        it "CalendarDiffTime"                    $ property $ \(a :: CalendarDiffTime)                     -> encodeText' a === JSON.encodeText a
        it "arbitrary message"                   $ property $ \(a :: Value)                                -> encodeText' a === JSON.encodeText a
