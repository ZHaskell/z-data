{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Data.Parser.UUIDSpec where

import qualified Data.List                as List
import           Data.Word
import           Data.Int
import           GHC.Float
import qualified Z.Data.Builder           as B
import qualified Z.Data.Parser            as P
import qualified Z.Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.QuickCheck.Instances.UUID
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "parser uuid" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "uuid roundtrip" $ \ t ->
            Right t === (P.parse' P.uuid . B.build $ B.uuid t)

        prop "uuid binary roundtrip" $ \ t ->
            Right t === (P.parse' P.decodeUUID . B.build $ B.encodeUUID t)