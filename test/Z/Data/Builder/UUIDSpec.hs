{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Data.Builder.UUIDSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Int
import           GHC.Float
import           Data.Time.Format
import qualified Z.Data.Builder.UUID      as B
import qualified Z.Data.Builder.Base      as B
import qualified Z.Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.QuickCheck.Instances.UUID
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "builder uuid" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "uuid === show" $ \ t ->
            (show t) === (T.unpack . B.buildText $ B.uuid t)

