{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Data.Builder.NumericSpec where

import qualified Data.List                  as List
import qualified Z.MonoList.IntList    as IntList
import           Data.Word
import           Data.Int
import           GHC.Float
import           Text.Printf                 (printf)
import           Foreign.C.Types
import qualified Z.Data.Builder.Numeric as B
import qualified Z.Data.Builder.Base    as B
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V
import qualified Data.Scientific as Sci
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.QuickCheck.Instances.Scientific
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "builder numeric" . modifyMaxSuccess (*50) . modifyMaxSize (*50) $ do
    describe "int roundtrip" $ do
        prop "Word roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Word i)
        prop "Int roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Int i)
        prop "Word64 roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Word64 i)
        prop "Int64 roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Int64 i)
        prop "Word32 roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Word32 i)
        prop "Int32 roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Int32 i)
        prop "Word16 roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Word16 i)
        prop "Int16 roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Int16 i)
        prop "Word8 roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Word8 i)
        prop "Int8 roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @Int8 i)
        prop "CShort roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @CShort i)
        prop "CUShort roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @CUShort i)
        prop "CInt roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @CInt i)
        prop "CUInt roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @CUInt i)
        prop "CLong roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @CLong i)
        prop "CULong roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @CULong i)
        prop "CLLong roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @CLLong i)
        prop "CULLong roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.int @CULLong i)

    describe "padding int roundtrip" $ do
        let f = B.defaultIFormat{B.width = 100, B.padding = B.ZeroPadding}
        prop "Word roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.intWith @Word f i)
        prop "Int roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.intWith @Int f i)
        prop "padding length" $ \ i ->
            100 === (V.length . B.build $ B.intWith @Word f i)
        prop "padding length" $ \ i ->
            100 === (V.length . B.build $ B.intWith @Int f i)

        let f = B.defaultIFormat{B.width = 10, B.padding = B.ZeroPadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.intWith @Int f i)

        let f = B.defaultIFormat{B.width = 10, B.padding = B.LeftSpacePadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.intWith @Int f i)

        let f = B.defaultIFormat{B.width = 10, B.padding = B.RightSpacePadding}
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.intWith @Word f i)
        prop "padding roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.intWith @Int f i)

    describe "c_intWith == hs_intWith" $ do
        prop "c_intWith == hs_intWith @Word" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Word f i)
        prop "c_intWith == hs_intWith @Word8" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Word8 f i)
        prop "c_intWith == hs_intWith @Word16" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Word16 f i)
        prop "c_intWith == hs_intWith @Word32" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Word32 f i)
        prop "c_intWith == hs_intWith @Word64" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Word64 f i)
        prop "c_intWith == hs_intWith @Int" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Int f i)
        prop "c_intWith == hs_intWith @Int8" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Int8 f i)
        prop "c_intWith == hs_intWith @Int16" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Int16 f i)
        prop "c_intWith == hs_intWith @Int32" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Int32 f i)
        prop "c_intWith == hs_intWith @Int64" $ \ i f ->
            (B.build $ B.hs_intWith f i) === (B.build $ B.c_intWith @Int64 f i)

    describe "integer roundtrip" $ do
        prop "integer roundtrip" $ \ i ->
            i === (read . T.unpack . B.buildText $ B.integer i)
        prop "integer roundtrip II" $
            -- there're an issue with leading zeros in front of an block, so we add a case manually here
            (2132132100000000000000000000000000213213 :: Integer) ===
                (read . T.unpack . B.buildText $ B.integer 2132132100000000000000000000000000213213)

    describe "scientific roundtrip" $ do
        prop "scientific roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . B.buildText . B.scientific $ Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . B.buildText . B.scientificWith B.Exponent Nothing $
                    Sci.scientific c e)
        {- B.Exponent doesn't roundtrip, i.e. B.scientificWith B.Exponent (Just 0) 101 ===> 1e2
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . B.buildText . B.scientificWith B.Exponent (Just (abs e)) $
                    Sci.scientific c e)
        -}
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . B.buildText . B.scientificWith B.Generic Nothing $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . B.buildText . B.scientificWith B.Generic (Just (abs e)) $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . B.buildText . B.scientificWith B.Fixed Nothing $
                    Sci.scientific c e)
        prop "scientificWith roundtrip" $ \ c e ->
            Sci.scientific c e ===
                (read . T.unpack . B.buildText . B.scientificWith B.Fixed (Just (abs e)) $
                    Sci.scientific c e)

    describe "hex roundtrip" $ do

        let read' s = read $ "0x" ++ s

        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Word i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Int i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Word64 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Int64 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Word32 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Int32 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Word16 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Int16 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Word8 i)
        prop "hex roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hex @Int8 i)

    describe "hexUpper roundtrip" $ do

        let read' s = read $ "0x" ++ s

        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Word i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Int i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Word64 i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Int64 i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Word32 i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Int32 i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Word16 i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Int16 i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Word8 i)
        prop "hexUpper roundtrip" $ \ i ->
            i === (read' . T.unpack . B.buildText $ B.hexUpper @Int8 i)

    describe "int === show" $ do
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Word i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Int i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Word64 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Int64 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Word32 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Int32 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Word16 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Int16 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Word8 i)
        prop "int === show" $ \ i ->
            show i === (T.unpack . B.buildText $ B.int @Int8 i)

    describe "intWith === printf" $ do
        prop "int === printf %d" $ \ i ->
            printf "%d" i ===
                (T.unpack . B.buildText $ B.intWith @Int B.defaultIFormat i)
        prop "int === printf %xxd" $ \ i (Positive w) ->
            printf ("%" ++ show w ++ "d") i ===
                (T.unpack . B.buildText $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.LeftSpacePadding, B.width = w} i)
        prop "int === printf %0xxd" $ \ i (Positive w) ->
            printf ("%0" ++ show w ++ "d") i ===
                (T.unpack . B.buildText $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.ZeroPadding, B.width = w} i)
        prop "int === printf %-xx%" $ \ i (Positive w) ->
            printf ("%-" ++ show w ++ "d") i ===
                (T.unpack . B.buildText $ B.intWith @Int B.defaultIFormat
                    {B.padding = B.RightSpacePadding, B.width = w} i)
        prop "hex === printf %08x" $ \ i ->
            printf "%08x" i ===
                (T.unpack . B.buildText $ B.hex @Int32 i)

    describe "float, double === show" $ do
        prop "float === show" $ \ i ->
            show i === (T.unpack . B.buildText  $ B.float i)
        prop "double === show" $ \ i ->
            show i === (T.unpack . B.buildText  $ B.double i)
        prop "scientific === show" $ \ i ->
            show i === (T.unpack . B.buildText  $ B.scientific i)

    describe "floatWith, doubleWith === formatRealFloat" $ do
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFGeneric l i ===
                (T.unpack . B.buildText  $ B.floatWith B.Generic l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFGeneric l i ===
                (T.unpack . B.buildText  $ B.doubleWith  B.Generic l i)
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFFixed  l  i ===
                (T.unpack . B.buildText  $ B.floatWith B.Fixed  l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFFixed  l  i ===
                (T.unpack . B.buildText  $ B.doubleWith  B.Fixed  l i)
        prop "floatWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFExponent l i ===
                (T.unpack . B.buildText  $ B.floatWith B.Exponent l i)
        prop "doubleWith === formatRealFloat" $ \ i l ->
            formatRealFloat FFExponent l i ===
                (T.unpack . B.buildText  $ B.doubleWith B.Exponent l i)

    describe "scientificWith === formatScientific" $ do
        prop "scientificWith === formatScientific" $ \ i (Positive l) ->
            Sci.formatScientific Sci.Generic (Just l) i ===
                (T.unpack . B.buildText  $ B.scientificWith B.Generic (Just l) i)
        prop "scientificWith === formatScientific" $ \ i l ->
            Sci.formatScientific Sci.Fixed l i ===
                (T.unpack . B.buildText  $ B.scientificWith  B.Fixed l i)
        prop "scientificWith === formatScientific" $ \ i (Positive l) ->
            Sci.formatScientific Sci.Exponent (Just l) i ===
                (T.unpack . B.buildText  $ B.scientificWith B.Exponent (Just l) i)

    describe "grisu3, grisu3_sp === floatToDigits 10" $ do
        prop "grisu3 === floatToDigits" $ \ (Positive f) ->
            let (ds, e) = B.grisu3 f
            in (IntList.toList ds, e) === floatToDigits 10 f
        prop "grisu3_sp === floatToDigits" $ \ (Positive f) ->
            let (ds, e) = B.grisu3_sp f
            in (IntList.toList ds, e) === floatToDigits 10 f
