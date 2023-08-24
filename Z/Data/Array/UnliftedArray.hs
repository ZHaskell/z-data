#if defined(CN_DOC)
{- |
Module      : Z.Array.UnliftedArray
Description : Unlifted 数组
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

对于 GHC 来说，有一类特殊的类型，比如'IORef', 'PrimArray' 等等，他们都是可以看作是打包了一个本身就是指针表示
的数据类型（见下面的'PrimUnlifted'），那么 'UnliftedArray'\/'MutableUnliftedArray' 就可以直接储存这些指针。
不建议直接使用该模块，请使用 'Z.Data.Array'。

-}
#else
{- |
Module      : Z.Array.UnliftedArray
Description : Unlifted arrays
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Arrays stores unlifted type without redirect. Please do not use this module directly, use "Z.Data.Array" instead.
-}
#endif
module Z.Data.Array.UnliftedArray where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import GHC.MVar (MVar(..))
import GHC.IORef (IORef(..))
import GHC.STRef (STRef(..))
import GHC.Conc (TVar(..))
import GHC.Exts

-- | Types with 'GHC.Exts.TYPE' 'GHC.Exts.UnliftedRep', which can be stored \/ retrieved in 'Array#'.
class PrimUnlifted a where
    -- | Corresponding unlifted Type
    type Unlifted a :: TYPE UnliftedRep
    -- | Remove extra box and get the unlifted pointer.
    primUnlift :: a -> Unlifted a
    -- | Add extra box so that can be used everywhere.
    primLift :: Unlifted a -> a

-- | Mutable array holding unlifted values.
data MutableUnliftedArray s a = MutableUnliftedArray (MutableArray# s (Unlifted a))

-- | Array holding unlifted values.
data UnliftedArray a = UnliftedArray (Array# (Unlifted a))

instance PrimUnlifted (UnliftedArray a) where
    type Unlifted (UnliftedArray a) = Array# (Unlifted a)
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (UnliftedArray a) = a
    primLift = UnliftedArray

instance PrimUnlifted (MutableUnliftedArray s a) where
    type Unlifted (MutableUnliftedArray s a) = MutableArray# s (Unlifted a)
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (MutableUnliftedArray a) = a
    primLift = MutableUnliftedArray

instance PrimUnlifted (Array a) where
    type Unlifted (Array a) = Array# a
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (Array a) = a
    primLift = Array

instance PrimUnlifted (MutableArray s a) where
    type Unlifted (MutableArray s a) = MutableArray# s a
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (MutableArray a) = a
    primLift = MutableArray

instance PrimUnlifted (SmallArray a) where
    type Unlifted (SmallArray a) = SmallArray# a
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (SmallArray a) = a
    primLift = SmallArray

instance PrimUnlifted (SmallMutableArray s a) where
    type Unlifted (SmallMutableArray s a) = SmallMutableArray# s a
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (SmallMutableArray a) = a
    primLift = SmallMutableArray

instance PrimUnlifted (PrimArray a) where
    type Unlifted (PrimArray a) = ByteArray#
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (PrimArray a) = a
    primLift = PrimArray

instance PrimUnlifted (MutablePrimArray s a) where
    type Unlifted (MutablePrimArray s a) = MutableByteArray# s
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (MutablePrimArray a) = a
    primLift = MutablePrimArray

instance PrimUnlifted ByteArray where
    type Unlifted ByteArray = ByteArray#
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (ByteArray a) = a
    primLift = ByteArray

instance PrimUnlifted (MutableByteArray s) where
    type Unlifted (MutableByteArray s) = MutableByteArray# s
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (MutableByteArray a) = a
    primLift = MutableByteArray

instance PrimUnlifted (MVar a) where
    type Unlifted (MVar a) = MVar# RealWorld a
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (MVar a) = a
    primLift = MVar

instance PrimUnlifted (TVar a) where
    type Unlifted (TVar a) = TVar# RealWorld a
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (TVar a) = a
    primLift = TVar

instance PrimUnlifted (STRef s a) where
    type Unlifted (STRef s a) = MutVar# s a
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (STRef a) = a
    primLift = STRef

instance PrimUnlifted (IORef a) where
    type Unlifted (IORef a) = MutVar# RealWorld a
    {-# INLINE primUnlift #-}
    {-# INLINE primLift #-}
    primUnlift (IORef (STRef a)) = a
    primLift a = IORef (STRef a)

--------------------------------------------------------------------------------


writeUnliftedArray :: (PrimUnlifted a, PrimMonad m)
    => MutableUnliftedArray (PrimState m) a
    -> Int
    -> a
    -> m ()
{-# INLINE writeUnliftedArray #-}
writeUnliftedArray (MutableUnliftedArray arr#) (I# i#) a = primitive (\ s0 ->
    let s1 = writeArray# arr# i# (primUnlift a) s0 in (# s1, () #))

readUnliftedArray :: (PrimUnlifted a, PrimMonad m)
    => MutableUnliftedArray (PrimState m) a
    -> Int
    -> m a
{-# INLINE readUnliftedArray #-}
readUnliftedArray (MutableUnliftedArray a) (I# i) = primitive ( \ s0 ->
    case readArray# a i s0 of (# s1, x #) -> (# s1, primLift x #))

indexUnliftedArray :: (PrimUnlifted a)
                   => UnliftedArray a
                   -> Int
                   -> a
{-# INLINE indexUnliftedArray #-}
indexUnliftedArray (UnliftedArray a) (I# i) =
    case (indexArray# a i) of (# x #) -> primLift x

indexUnliftedArray' :: (PrimUnlifted a)
                   => UnliftedArray a
                   -> Int
                   -> (# a #) 
{-# INLINE indexUnliftedArray' #-}
indexUnliftedArray' (UnliftedArray a) (I# i) =
    case (indexArray# a i) of (# x #) -> (# primLift x #)

indexUnliftedArrayM :: (PrimUnlifted a, Monad m)
                   => UnliftedArray a
                   -> Int
                   -> m a
{-# INLINE indexUnliftedArrayM #-}
indexUnliftedArrayM (UnliftedArray a) (I# i) =
    case (indexArray# a i) of (# x #) -> return (primLift x)

unsafeNewUnliftedArray
    :: PrimMonad m
    => Int -- ^ size
    -> m (MutableUnliftedArray (PrimState m) a)
{-# INLINE unsafeNewUnliftedArray #-}
unsafeNewUnliftedArray (I# i#) = primitive (\ s1 -> 
    case emptyUnliftedArray @ByteArray of
        UnliftedArray placeholder -> case newArray# i# (unsafeCoerce# placeholder) s1 of
            (# s2, maa# #) -> (# s2, MutableUnliftedArray maa# #))

emptyUnliftedArray :: (PrimUnlifted a) => UnliftedArray a
{-# NOINLINE emptyUnliftedArray #-}
emptyUnliftedArray = runST (do
    mua <- unsafeNewUnliftedArray 0
    unsafeFreezeUnliftedArray mua)

newUnliftedArray
    :: (PrimUnlifted a, PrimMonad m)
    => Int -- ^ size
    -> a -- ^ initial value
    -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray (I# i#) v = primitive (\ s -> 
    case newArray# i# (primUnlift v) s of
        (# s', maa# #) -> (# s', MutableUnliftedArray maa# #))
{-# INLINE newUnliftedArray #-}

setUnliftedArray
    :: (PrimUnlifted a, PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset
    -> Int -- ^ length
    -> a -- ^ value to fill with
    -> m ()
{-# INLINE setUnliftedArray #-}
setUnliftedArray mua off len v = loop (len + off - 1)
  where
    loop i
        | i < off = pure ()
        | otherwise = writeUnliftedArray mua i v *> loop (i-1)

sizeofUnliftedArray :: UnliftedArray e -> Int
{-# INLINE sizeofUnliftedArray #-}
sizeofUnliftedArray (UnliftedArray aa#) = I# (sizeofArray# aa#)

sizeofMutableUnliftedArray :: MutableUnliftedArray s e -> Int
{-# INLINE sizeofMutableUnliftedArray #-}
sizeofMutableUnliftedArray (MutableUnliftedArray maa#)
    = I# (sizeofMutableArray# maa#)

unsafeThawUnliftedArray 
    :: PrimMonad m
    => UnliftedArray a -> m (MutableUnliftedArray (PrimState m) a)
{-# INLINE unsafeThawUnliftedArray #-}
unsafeThawUnliftedArray (UnliftedArray a)
    = primitive $ \s -> case unsafeThawArray# a s of
        (# s',ma #) -> (# s', MutableUnliftedArray ma #)


unsafeFreezeUnliftedArray 
    :: PrimMonad m
    => MutableUnliftedArray (PrimState m) a -> m (UnliftedArray a)
unsafeFreezeUnliftedArray (MutableUnliftedArray maa#)
    = primitive $ \s -> case unsafeFreezeArray# maa# s of
        (# s', aa# #) -> (# s', UnliftedArray aa# #)
{-# INLINE unsafeFreezeUnliftedArray #-}

sameMutableUnliftedArray
    :: MutableUnliftedArray s a
    -> MutableUnliftedArray s a
    -> Bool
sameMutableUnliftedArray (MutableUnliftedArray maa1#) (MutableUnliftedArray maa2#)
    = isTrue# (unsafePtrEquality# maa1# maa2#)
{-# INLINE sameMutableUnliftedArray #-}

copyUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset into destination
    -> UnliftedArray a -- ^ source
    -> Int -- ^ offset into source
    -> Int -- ^ number of elements to copy
    -> m ()
{-# INLINE copyUnliftedArray #-}
copyUnliftedArray
    (MutableUnliftedArray dst) (I# doff)
    (UnliftedArray src) (I# soff) (I# ln) =
      primitive_ $ copyArray# src soff dst doff ln


copyMutableUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ destination
    -> Int -- ^ offset into destination
    -> MutableUnliftedArray (PrimState m) a -- ^ source
    -> Int -- ^ offset into source
    -> Int -- ^ number of elements to copy
    -> m ()
{-# INLINE copyMutableUnliftedArray #-}
copyMutableUnliftedArray
    (MutableUnliftedArray dst) (I# doff)
    (MutableUnliftedArray src) (I# soff) (I# ln) =
      primitive_ $ copyMutableArray# src soff dst doff ln

freezeUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (UnliftedArray a)
freezeUnliftedArray src off len = do
    dst <- unsafeNewUnliftedArray len
    copyMutableUnliftedArray dst 0 src off len
    unsafeFreezeUnliftedArray dst
{-# INLINE freezeUnliftedArray #-}

thawUnliftedArray
    :: (PrimMonad m)
    => UnliftedArray a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (MutableUnliftedArray (PrimState m) a)
{-# INLINE thawUnliftedArray #-}
thawUnliftedArray src off len = do
    dst <- unsafeNewUnliftedArray len
    copyUnliftedArray dst 0 src off len
    return dst

cloneUnliftedArray
    :: UnliftedArray a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> UnliftedArray a
{-# INLINE cloneUnliftedArray #-}
cloneUnliftedArray src off len = runST $ do
    dst <- unsafeNewUnliftedArray len
    copyUnliftedArray dst 0 src off len
    unsafeFreezeUnliftedArray dst

cloneMutableUnliftedArray
    :: (PrimMonad m)
    => MutableUnliftedArray (PrimState m) a -- ^ source
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (MutableUnliftedArray (PrimState m) a)
{-# INLINE cloneMutableUnliftedArray #-}
cloneMutableUnliftedArray src off len = do
    dst <- unsafeNewUnliftedArray len
    copyMutableUnliftedArray dst 0 src off len
    return dst
