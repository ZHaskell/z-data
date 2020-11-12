module Z.Data.Text.HexBytesSpec where

import           Data.Word
import qualified Z.Data.Text.HexBytes     as T
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "text-HexBytes" $ do
    describe "encoding props " $ do
        prop "hexDecode' . hexEncode === id" $ \ xs ->
            (T.hexDecode' (T.hexEncode xs) === xs)
