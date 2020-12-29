{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Data.Builder.TimeSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Int
import           GHC.Float
import           Data.Time.Format
import qualified Z.Data.Builder.Time      as B
import qualified Z.Data.Builder.Base      as B
import qualified Z.Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.QuickCheck.Instances.Time
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Data.Time.LocalTime

spec :: Spec
spec = describe "builder time" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
    describe "utcTime === format" $ do
        prop "utcTime === format" $ \ t ->
            (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" t) === (T.unpack . B.buildText $ B.utcTime t)

    describe "localTime === format" $ do
        prop "localTime === format" $ \ t ->
            (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" t) === (T.unpack . B.buildText $ B.localTime t)

    describe "zonedTime === format" $ do
        prop "zonedTime === format" $ \ t0 z0 ->
            let z = abs z0 `div` 1440
                t = ZonedTime t0 (minutesToTimeZone z)
            in if z == 0
                then (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" t) === (T.unpack . B.buildText $ B.zonedTime t)
                else (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" t) === (T.unpack . B.buildText $ B.zonedTime t)
