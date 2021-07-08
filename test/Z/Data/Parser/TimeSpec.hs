{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Data.Parser.TimeSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Int
import           GHC.Float
import           Data.Time.Format
import qualified Z.Data.Builder           as B
import qualified Z.Data.Parser            as P
import qualified Z.Data.Parser.Time       as P
import qualified Z.Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.QuickCheck.Instances.Time
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Data.Time.LocalTime
import           Data.Time.Calendar

spec :: Spec
spec = describe "parser time" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "utcTime roundtrip" $ \ t ->
            Right t === (P.parse' P.utcTime . B.build $ B.utcTime t)

        prop "localTime roundtrip" $ \ t ->
            Right t === (P.parse' P.localTime . B.build $ B.localTime t)

        prop "zonedTime roundtrip" $ \ t0 z0 ->
            let z = abs z0 `div` 1440
                t = ZonedTime t0 (minutesToTimeZone z)
            in (zonedTimeToUTC <$> Right t) === (zonedTimeToUTC <$> (P.parse' P.zonedTime . B.build $ B.zonedTime t))

        modifyMaxSuccess (*100) . modifyMaxSize (*100) . prop "fromGregorianValidInt64 == fromGregorianValid" $ \ y m d ->
            P.fromGregorianValid' y m d == fromGregorianValid y m d
