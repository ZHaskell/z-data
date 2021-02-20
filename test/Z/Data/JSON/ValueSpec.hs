{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Data.JSON.ValueSpec where

import qualified Data.List                as L
import           Data.Word
import           Data.Int
import           GHC.Float
import qualified Z.Data.Builder         as B
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Data.Scientific as Sci
import qualified Z.Data.JSON.Value as JSON
import qualified Z.Data.JSON.Builder as JSONB


spec :: Spec
spec = describe "JSON" $ do -- large size will generate too huge JSON document
    prop "value roundtrip" $ \ v ->
        Right v === JSON.parseValue' (B.build (JSONB.value v))

    describe "floatToScientific, doubleToScientific === fromFloatDigits"  $ do
        prop "floatToScientific == fromFloatDigits" $ \ i ->
            JSON.floatToScientific i === Sci.fromFloatDigits i
        prop "floatToScientific === fromFloatDigits" $ \ i ->
            JSON.doubleToScientific i === Sci.fromFloatDigits i
