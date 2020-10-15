{-|
Module      : Z.Data.Generics.Utils
Description : Compute Generic product size during compile time
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides a helper class to compute product size via 'Generic' instance, e.g. This class is
useful during JSON deserializing, to decide the array length used to store record KVs.
-}

{-# LANGUAGE UndecidableInstances #-}

module Z.Data.Generics.Utils
  ( ProductSize(..)
  , productSize
  ) where

import GHC.Generics
import GHC.TypeNats
import GHC.Exts (Proxy#, proxy#)

-- | type class for calculating product size.
class KnownNat (PSize f) => ProductSize (f :: * -> *) where
    type PSize f :: Nat

instance ProductSize (S1 s a) where
    type PSize (S1 s a) = 1
instance (KnownNat (PSize a + PSize b), ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    type PSize (a :*: b) = PSize a + PSize b

productSize :: forall f. KnownNat (PSize f) => Proxy# f -> Int
productSize _ = fromIntegral (natVal' (proxy# :: Proxy# (PSize f)))
