{-# OPTIONS_GHC -fno-warn-orphans #-}

module Z.Data.Utils.Orphan.Prim where

import Data.Complex (Complex(..))
import Data.Primitive.Types (Prim(..), defaultSetOffAddr#, defaultSetByteArray#)
import GHC.Exts
import GHC.Real (Ratio(..))

instance Prim a => Prim (Complex a) where
  sizeOf# _ = 2# *# sizeOf# (undefined :: a)
  alignment# _ = alignment# (undefined :: a)
  indexByteArray# arr# i# =
    let x,y :: a
        x = indexByteArray# arr# (2# *# i#)
        y = indexByteArray# arr# (2# *# i# +# 1#)
    in x :+ y
  readByteArray# arr# i# =
    \s0 -> case readByteArray# arr# (2# *# i#) s0 of
      (# s1#, x #) -> case readByteArray# arr# (2# *# i# +# 1#) s1# of
        (# s2#, y #) -> (# s2#, x :+ y #)
  writeByteArray# arr# i# (a :+ b) =
    \s0 -> case writeByteArray# arr# (2# *# i#) a s0 of
      s1 -> case writeByteArray# arr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setByteArray# = defaultSetByteArray#
  indexOffAddr# addr# i# =
    let x,y :: a
        x = indexOffAddr# addr# (2# *# i#)
        y = indexOffAddr# addr# (2# *# i# +# 1#)
    in x :+ y
  readOffAddr# addr# i# =
    \s0 -> case readOffAddr# addr# (2# *# i#) s0 of
      (# s1, x #) -> case readOffAddr# addr# (2# *# i# +# 1#) s1 of
        (# s2, y #) -> (# s2, x :+ y #)
  writeOffAddr# addr# i# (a :+ b) =
    \s0 -> case writeOffAddr# addr# (2# *# i#) a s0 of
      s1 -> case writeOffAddr# addr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setOffAddr# = defaultSetOffAddr#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}

instance (Integral a, Prim a) => Prim (Ratio a) where
  sizeOf# _ = 2# *# sizeOf# (undefined :: a)
  alignment# _ = alignment# (undefined :: a)
  indexByteArray# arr# i# =
    let x,y :: a
        x = indexByteArray# arr# (2# *# i#)
        y = indexByteArray# arr# (2# *# i# +# 1#)
    in x :% y
  readByteArray# arr# i# =
    \s0 -> case readByteArray# arr# (2# *# i#) s0 of
      (# s1#, x #) -> case readByteArray# arr# (2# *# i# +# 1#) s1# of
        (# s2#, y #) -> (# s2#, x :% y #)
  writeByteArray# arr# i# (a :% b) =
    \s0 -> case writeByteArray# arr# (2# *# i#) a s0 of
      s1 -> case writeByteArray# arr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setByteArray# = defaultSetByteArray#
  indexOffAddr# addr# i# =
    let x,y :: a
        x = indexOffAddr# addr# (2# *# i#)
        y = indexOffAddr# addr# (2# *# i# +# 1#)
    in x :% y
  readOffAddr# addr# i# =
    \s0 -> case readOffAddr# addr# (2# *# i#) s0 of
      (# s1, x #) -> case readOffAddr# addr# (2# *# i# +# 1#) s1 of
        (# s2, y #) -> (# s2, x :% y #)
  writeOffAddr# addr# i# (a :% b) =
    \s0 -> case writeOffAddr# addr# (2# *# i#) a s0 of
      s1 -> case writeOffAddr# addr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setOffAddr# = defaultSetOffAddr#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}
