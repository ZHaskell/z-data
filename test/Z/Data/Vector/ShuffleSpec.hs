{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Data.Vector.ShuffleSpec where

import qualified Z.Data.Vector.Shuffle  as V
import qualified Z.Data.Vector          as V
import Data.List ( sort )
import Test.QuickCheck.Monadic ( assert, monadicIO, run )
import System.Random ( mkStdGen )
import System.Random.Stateful ( mkStdGen, newIOGenM )
import Test.Hspec ( describe, Spec )
import Test.Hspec.QuickCheck ( prop )

spec :: Spec
spec = describe "vector-shuffle " $ do
    describe "Shuffle props " $ do
         prop "sort . shuffle === sort" $ \ xs ->
             monadicIO $ do 
                  (x, y) <- run $ do   
                     x <- V.mergeSort <$> (newIOGenM (mkStdGen 111) >>= V.shuffle (V.pack @V.Vector @Integer xs))
                     return (x, V.pack (sort xs))
                  assert (x == y)

