{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Data.Parser.NumericSpec where

import qualified Data.List                as L
import           Data.Word
import           Data.Int
import           GHC.Float
import           Text.Printf                 (printf)
import qualified Z.Data.Parser.Numeric    as P
import qualified Z.Data.Parser.Base    as P
import qualified Z.Data.Builder.Numeric    as B
import qualified Z.Data.Builder.Base    as B
import qualified Z.Data.Text as T
import qualified Z.Data.Vector.Base as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "numeric parsers roundtrip" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Int)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Int64)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Int32)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Int16)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Int8)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Word)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Word64)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Word32)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Word16)
        prop "positive hex roundtrip" $ \ i ->
            P.parse' P.hex (B.build (B.hex i)) === Right (i :: Word8)


        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Int)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Int64)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Int32)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Int16)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Int8)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Word)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Word64)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Word32)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Word16)
        prop "positive int roundtrip" $ \ (Positive i) ->
            P.parse' P.uint (B.build (B.int i)) === Right (i :: Word8)


        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Int)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Int64)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Int32)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Int16)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Int8)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Word)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Word64)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Word32)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Word16)
        prop "positive int roundtrip" $ \ i ->
            P.parse' P.int (B.build (B.int i)) === Right (i :: Word8)

        prop "float roundtrip" $ \ i ->
            P.parse' P.float (B.build (B.float i)) === Right (i :: Float)
        prop "double roundtrip" $ \ i ->
            P.parse' P.double (B.build (B.double i)) === Right (i :: Double)
