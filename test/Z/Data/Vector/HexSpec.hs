module Z.Data.Vector.HexSpec where

import           Data.Word
import qualified Z.Data.Vector.Hex     as H
import qualified Z.Data.Vector         as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "text-HexBytes" $ do
    describe "encoding props " $ do
        prop "hexDecode' . hexEncode === id" $ \ xs u ->
            (H.hexDecode' (H.hexEncode u xs) === xs)
