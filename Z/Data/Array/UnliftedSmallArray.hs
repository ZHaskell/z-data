{- |
Module      : Z.Array.UnliftedSmallArray
Description : Small unlifted primitve arrays
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

#ifdef CN_DOC
'UnliftedArray' 的 'SmallArray' 版本。
#else
SmallArray version of 'UnliftedArray'. Please do not use this module directly, use "Z.Array" instead.
#endif

-}
module Z.Data.Array.UnliftedSmallArray where

import Control.Monad.Primitive
import Data.Primitive.SmallArray
import GHC.Exts
import GHC.ST
import Z.Data.Array.UnliftedArray

-- | Small mutable array holding unlifted values.
data MutableUnliftedSmallArray s a = MutableUnliftedSmallArray (SmallMutableArray# s (Unlifted a))

-- | Small array holding unlifted values.
data UnliftedSmallArray a = UnliftedSmallArray (SmallArray# (Unlifted a))

instance PrimUnlifted (MutableUnliftedSmallArray s a) where
    type Unlifted (MutableUnliftedSmallArray s a) = SmallMutableArray# s (Unlifted a)
    primUnlift (MutableUnliftedSmallArray arr) = arr
    {-# INLINE primUnlift #-}
    primLift = MutableUnliftedSmallArray
    {-# INLINE primLift #-}

instance PrimUnlifted (UnliftedSmallArray a) where
    type Unlifted (UnliftedSmallArray a) = SmallArray# (Unlifted a)
    primUnlift (UnliftedSmallArray arr) = arr
    {-# INLINE primUnlift #-}
    primLift = UnliftedSmallArray
    {-# INLINE primLift #-}

--------------------------------------------------------------------------------

writeUnliftedSmallArray :: (PrimUnlifted a, PrimMonad m)
    => MutableUnliftedSmallArray (PrimState m) a
    -> Int
    -> a
    -> m ()
{-# INLINE writeUnliftedSmallArray #-}
writeUnliftedSmallArray (MutableUnliftedSmallArray arr#) (I# i#) a = primitive (\ s0 ->
    let s1 = writeSmallArray# arr# i# (primUnlift a) s0 in (# s1, () #))

readUnliftedSmallArray :: (PrimUnlifted a, PrimMonad m)
    => MutableUnliftedSmallArray (PrimState m) a
    -> Int
    -> m a
{-# INLINE readUnliftedSmallArray #-}
readUnliftedSmallArray (MutableUnliftedSmallArray a) (I# i) = primitive ( \ s0 ->
    case readSmallArray# a i s0 of (# s1, x #) -> (# s1, primLift x #))

indexUnliftedSmallArray :: (PrimUnlifted a)
                   => UnliftedSmallArray a
                   -> Int
                   -> a
{-# INLINE indexUnliftedSmallArray #-}
indexUnliftedSmallArray (UnliftedSmallArray a) (I# i) =
    case (indexSmallArray# a i) of (# x #) -> primLift x

indexUnliftedSmallArray' :: (PrimUnlifted a)
                   => UnliftedSmallArray a
                   -> Int
                   -> (# a #) 
{-# INLINE indexUnliftedSmallArray' #-}
indexUnliftedSmallArray' (UnliftedSmallArray a) (I# i) =
    case (indexSmallArray# a i) of (# x #) -> (# primLift x #)

indexUnliftedSmallArrayM :: (PrimUnlifted a, Monad m)
                   => UnliftedSmallArray a
                   -> Int
                   -> m a
{-# INLINE indexUnliftedSmallArrayM #-}
indexUnliftedSmallArrayM (UnliftedSmallArray a) (I# i) =
    case (indexSmallArray# a i) of (# x #) -> return $! primLift x

unsafeNewUnliftedSmallArray
    :: PrimMonad m
    => Int -- ^ size
    -> m (MutableUnliftedSmallArray (PrimState m) a)
{-# INLINE unsafeNewUnliftedSmallArray #-}
unsafeNewUnliftedSmallArray (I# i#) = primitive (\ s1 -> 
    case emptyUnliftedSmallArray @(SmallArray Int) of
        UnliftedSmallArray placeholder -> case newSmallArray# i# (unsafeCoerce# placeholder) s1 of
            (# s2, maa# #) -> (# s2, MutableUnliftedSmallArray maa# #))

emptyUnliftedSmallArray :: (PrimUnlifted a) => UnliftedSmallArray a
{-# NOINLINE emptyUnliftedSmallArray #-}
emptyUnliftedSmallArray = runST (do
    mua <- unsafeNewUnliftedSmallArray 0
    unsafeFreezeUnliftedSmallArray mua)

newUnliftedSmallArray
    :: (PrimUnlifted a, PrimMonad m)
    => Int -- ^ size
    -> a -- ^ initial value
    -> m (MutableUnliftedSmallArray (PrimState m) a)
newUnliftedSmallArray (I# i#) v = primitive (\ s -> 
    case newSmallArray# i# (primUnlift v) s of
        (# s', maa# #) -> (# s', MutableUnliftedSmallArray maa# #))
{-# INLINE newUnliftedSmallArray #-}

setUnliftedSmallArray
    :: (PrimUnlifted a, PrimMonad m)
    => MutableUnliftedSmallArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset
    -> Int -- ^ length
    -> a -- ^ value to fill with
    -> m ()
{-# INLINE setUnliftedSmallArray #-}
setUnliftedSmallArray mua off len v = loop (len + off - 1)
  where
    loop i
        | i < off = pure ()
        | otherwise = writeUnliftedSmallArray mua i v *> loop (i-1)

sizeofUnliftedSmallArray :: UnliftedSmallArray e -> Int
{-# INLINE sizeofUnliftedSmallArray #-}
sizeofUnliftedSmallArray (UnliftedSmallArray aa#) = I# (sizeofSmallArray# aa#)

sizeofMutableUnliftedSmallArray :: MutableUnliftedSmallArray s e -> Int
{-# INLINE sizeofMutableUnliftedSmallArray #-}
sizeofMutableUnliftedSmallArray (MutableUnliftedSmallArray maa#)
    = I# (sizeofSmallMutableArray# maa#)

unsafeThawUnliftedSmallArray 
    :: PrimMonad m
    => UnliftedSmallArray a -> m (MutableUnliftedSmallArray (PrimState m) a)
{-# INLINE unsafeThawUnliftedSmallArray #-}
unsafeThawUnliftedSmallArray (UnliftedSmallArray a)
    = primitive $ \s -> case unsafeThawSmallArray# a s of
        (# s',ma #) -> (# s', MutableUnliftedSmallArray ma #)


unsafeFreezeUnliftedSmallArray 
    :: PrimMonad m
    => MutableUnliftedSmallArray (PrimState m) a -> m (UnliftedSmallArray a)
unsafeFreezeUnliftedSmallArray (MutableUnliftedSmallArray maa#)
    = primitive $ \s -> case unsafeFreezeSmallArray# maa# s of
        (# s', aa# #) -> (# s', UnliftedSmallArray aa# #)
{-# INLINE unsafeFreezeUnliftedSmallArray #-}

sameMutableUnliftedSmallArray
    :: MutableUnliftedSmallArray s a
    -> MutableUnliftedSmallArray s a
    -> Bool
sameMutableUnliftedSmallArray (MutableUnliftedSmallArray maa1#) (MutableUnliftedSmallArray maa2#)
    = isTrue# (unsafePtrEquality# maa1# maa2#)
{-# INLINE sameMutableUnliftedSmallArray #-}

copyUnliftedSmallArray
    :: (PrimMonad m)
    => MutableUnliftedSmallArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset into destination
    -> UnliftedSmallArray a -- ^ source
    -> Int -- ^ offset into source
    -> Int -- ^ number of elements to copy
    -> m ()
{-# INLINE copyUnliftedSmallArray #-}
copyUnliftedSmallArray
    (MutableUnliftedSmallArray dst) (I# doff)
    (UnliftedSmallArray src) (I# soff) (I# ln) =
      primitive_ $ copySmallArray# src soff dst doff ln


copyMutableUnliftedSmallArray
    :: (PrimMonad m)
    => MutableUnliftedSmallArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset into destination
    -> MutableUnliftedSmallArray (PrimState m) a -- ^ source
    -> Int -- ^ offset into source
    -> Int -- ^ number of elements to copy
    -> m ()
{-# INLINE copyMutableUnliftedSmallArray #-}
copyMutableUnliftedSmallArray
    (MutableUnliftedSmallArray dst) (I# doff)
    (MutableUnliftedSmallArray src) (I# soff) (I# ln) =
      primitive_ $ copySmallMutableArray# src soff dst doff ln

freezeUnliftedSmallArray
    :: (PrimMonad m)
    => MutableUnliftedSmallArray (PrimState m) a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (UnliftedSmallArray a)
freezeUnliftedSmallArray src off len = do
    dst <- unsafeNewUnliftedSmallArray len
    copyMutableUnliftedSmallArray dst 0 src off len
    unsafeFreezeUnliftedSmallArray dst
{-# INLINE freezeUnliftedSmallArray #-}


thawUnliftedSmallArray
    :: (PrimMonad m)
    => UnliftedSmallArray a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (MutableUnliftedSmallArray (PrimState m) a)
{-# INLINE thawUnliftedSmallArray #-}
thawUnliftedSmallArray src off len = do
    dst <- unsafeNewUnliftedSmallArray len
    copyUnliftedSmallArray dst 0 src off len
    return dst

cloneUnliftedSmallArray
    :: UnliftedSmallArray a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> UnliftedSmallArray a
{-# INLINE cloneUnliftedSmallArray #-}
cloneUnliftedSmallArray src off len = runST $ do
    dst <- unsafeNewUnliftedSmallArray len
    copyUnliftedSmallArray dst 0 src off len
    unsafeFreezeUnliftedSmallArray dst

cloneMutableUnliftedSmallArray
    :: (PrimMonad m)
    => MutableUnliftedSmallArray (PrimState m) a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (MutableUnliftedSmallArray (PrimState m) a)
{-# INLINE cloneMutableUnliftedSmallArray #-}
cloneMutableUnliftedSmallArray src off len = do
    dst <- unsafeNewUnliftedSmallArray len
    copyMutableUnliftedSmallArray dst 0 src off len
    return dst

shrinkMutableUnliftedSmallArray :: (PrimMonad m)
  => MutableUnliftedSmallArray (PrimState m) a
  -> Int
  -> m ()
{-# inline shrinkMutableUnliftedSmallArray #-}
shrinkMutableUnliftedSmallArray (MutableUnliftedSmallArray x) (I# n) = primitive
  (\s0 -> case shrinkSmallMutableArray# x n s0 of
    s1 -> (# s1, () #)
  )
