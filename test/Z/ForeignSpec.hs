{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.ForeignSpec where

import qualified Data.List                  as List
import qualified Z.Data.CBytes              as CB
import qualified Z.Data.JSON                as JSON
import           Z.Foreign
import qualified Z.Data.Vector.Base         as V
import qualified Z.Data.Array               as A
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           System.IO.Unsafe

spec :: Spec
spec = describe "Foreign" $ do
    describe "pass prim array list to foreign" $ do
        prop "sum first should be equal(unsafe FFI)" $ \ xss ->
            let pas = List.map (V.pack . getNonEmpty) xss :: [A.PrimArray Int]
                s = sum (List.map (List.head . getNonEmpty) xss)
            in unsafeDupablePerformIO (withPrimArrayListUnsafe pas sum_first_unsafe) === s

        prop "sum first should be equal(safe FFI)" $ \ xss ->
            let pas = List.map (V.pack . getNonEmpty) xss :: [A.PrimArray Int]
                s = sum (List.map (List.head . getNonEmpty) xss)
            in unsafeDupablePerformIO (withPrimArrayListSafe pas sum_first_safe) === s

--------------------------------------------------------------------------------

foreign import ccall unsafe sum_first_unsafe :: BAArray# Int -> Int -> IO Int
foreign import ccall safe sum_first_safe :: Ptr (Ptr Int) -> Int -> IO Int
