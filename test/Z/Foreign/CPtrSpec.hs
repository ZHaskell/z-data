{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.Foreign.CPtrSpec where

import           Foreign.Ptr
import qualified Data.List                  as List
import qualified Z.Data.CBytes              as CB
import qualified Z.Data.JSON                as JSON
import           Z.Foreign
import qualified Z.Data.Vector.Base         as V
import qualified Z.Data.Array               as A
import           Z.Foreign.CPtr
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           System.IO.Unsafe

spec :: Spec
spec = describe "Foreign.CPtr" $ do
    it "pass cptr list to foreign" $ do
        foo1 <- newCPtr' (malloc_foo 8) free_foo
        foo2 <- newCPtr' (malloc_foo 8) free_foo
        foo3 <- newCPtr' (malloc_foo 8) free_foo

        s1 <- withCPtrsUnsafe [foo1, foo2, foo3] sum_pointer
        s2 <- withCPtrs [foo1, foo2, foo3] sum_pointer_safe

        s3 <- withCPtr foo1 $ \ p1 ->
            withCPtr foo2 $ \ p2 ->
                withCPtr foo3 $ \ p3 -> do
                    let WordPtr p1' = ptrToWordPtr p1
                        WordPtr p2' = ptrToWordPtr p2
                        WordPtr p3' = ptrToWordPtr p3
                    return (p1'+p2'+p3')
        s1 @=? s3
        s2 @=? s3

--------------------------------------------------------------------------------

data Foo

foreign import ccall unsafe "malloc" malloc_foo :: CSize -> IO (Ptr Foo)
foreign import ccall unsafe "&free" free_foo :: FunPtr (Ptr Foo -> IO ())

foreign import ccall unsafe "sum_pointer" sum_pointer :: BA# (Ptr Foo) -> Int -> IO Word
foreign import ccall safe "sum_pointer" sum_pointer_safe :: (Ptr (Ptr Foo)) -> Int -> IO Word
