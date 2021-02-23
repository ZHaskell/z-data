module Z.Data.Vector.Base64Spec where

import           Data.Word
import qualified Z.Data.Vector.Base64   as B
import qualified Z.Data.Vector          as V
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "text-Base64Bytes" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
    describe "encoding props " $ do
        prop "base64Decode' . base64Encode === id" $ \ xs ->
            (B.base64Decode' (B.base64Encode xs) === xs)
