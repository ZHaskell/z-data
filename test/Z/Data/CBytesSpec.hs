{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Z.Data.CBytesSpec where

import           Data.Char                (ord)
import           Data.Hashable            (hash, hashWithSalt)
import qualified Data.List                as List
import           Data.Word
import qualified GHC.Exts                 as List
import           System.IO.Unsafe
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import qualified Z.Data.CBytes            as CB
import qualified Z.Data.JSON              as JSON
import qualified Z.Data.Vector.Base       as V
import           Z.Foreign

spec :: Spec
spec = describe "CBytes-base" $ do
    describe "CBytes Eq Ord property" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "CBytes eq === List.eq" $ \ xs ys ->
            (CB.pack xs == CB.pack ys) === (xs == ys)

        prop "CBytes compare === List.compare" $ \ xs ys ->
            let xs' = List.filter (/= '\NUL') xs
                ys' = List.filter (/= '\NUL') ys
            in (CB.pack xs' `compare` CB.pack ys') === (xs' `compare` ys')

    describe "CBytes Hashable instance property" . modifyMaxSuccess (*10) . modifyMaxSize (*10) $ do
        prop "CBytes a's hash should be equal to Bytes's hash" $ \ (ASCIIString xs) ->
            let ys = List.filter (/= '\NUL') xs
            in hash (CB.pack ys) === hash (V.packASCII ys)
        prop "CBytes a's hash should be equal to literal's hash" $
            hash ("hello world!" :: CB.CBytes) === hash (CB.fromBytes "hello world!")

    describe "CBytes JSON instance property" $ do
        prop "CBytes decodeJSON . encodeJSON === id" $ \ xs ->
            let bs = CB.fromBytes (V.pack xs)
            in Right bs === JSON.decode' (JSON.encode bs)

    describe "CBytes IsString instance property" $ do
        prop "ASCII string" $
            "hello world" === CB.fromText "hello world"
        prop "ASCII string" $
            "hello world" === CB.fromBytes "hello world"
        prop "ASCII string" $
           CB.toBytes "\NUL" === (V.pack [0xC0, 0x80])
        prop "UTF8 string" $
            "你好世界" === CB.fromText "你好世界"
        prop "UTF8 string" $
            "\NUL" === CB.pack ['\NUL']

    describe "CBytes length == List.length" $ do
        prop "CBytes length === List.length" $ \ (ASCIIString xs) ->
            let ys = List.filter (/= '\NUL') xs
            in (CB.length $ CB.pack ys)  ===  List.length ys

    describe "CBytes append == List.(++)" $ do
        prop "CBytes eq === List.eq" $ \ xs ys ->
            (CB.pack xs `CB.append` CB.pack ys) === CB.pack (xs ++ ys)

    describe "CBytes concat == List.concat" $ do
        prop "CBytes eq === List.eq" $ \ xss ->
            (CB.concat  (map CB.pack xss)) === CB.pack (List.concat xss)

    describe "withCBytes fromCString == id" $ do
        prop "withCBytes fromCString == id" $ \ xs ->
            (unsafeDupablePerformIO $ CB.withCBytes (CB.pack xs) (CB.fromCString . castPtr))
                === CB.pack xs

    describe "withCBytes fromNullTerminated  == toBytes" $ do
        prop "CBytes eq === List.eq" $ \ xs ->
            CB.toBytes (CB.pack xs) ===
                (unsafeDupablePerformIO $ CB.withCBytes (CB.pack xs) fromNullTerminated)

    describe "CBytes.fromPrimArray" $ do
        prop "CBytes pack === CBytes fromPrimArray" $ \(ASCIIString xs) ->
            let xs' = List.filter (/= '\NUL') xs
             in CB.pack xs' === CB.fromPrimArray (primArrayFromList $ map (fromIntegral . ord) xs')

    describe "CBytes.fromMutablePrimArray" $ do
        prop "CBytes pack === CBytes fromMutablePrimArray" $ \(ASCIIString xs) ->
            let xs' = List.filter (/= '\NUL') xs
             in unsafeDupablePerformIO $ do
                 marr <- unsafeThawPrimArray (primArrayFromList $ map (fromIntegral . ord) xs')
                 cb <- CB.fromMutablePrimArray marr
                 return $ CB.pack xs' === cb
