module Z.Data.Array.Buffer where

import Z.Data.PrimRef

data Buffer marr s a = Buffer
    { bufferArray :: {-# UNPACK #-} UnliftedRef s (marr s a)
    , writeIndex  :: {-# UNPACK #-} PrimRef s Int
    }

newBuffer :: PrimMonad m
          => Int                            -- ^ initial capacity
          -> m (Buffer (PrimState m) a)
newBuffer cap = do

size ::  Buffer (PrimState m) a -> m Int

capacity :: Buffer (PrimState m) a -> m Int

reserve :: Buffer (PrimState m) a -> Int ->  m ()

push :: PrimMonad m => Buffer (PrimState m) a -> a -> m ()

pushList :: PrimMonad m => Buffer (PrimState m) a -> [a] -> m ()

pushVec :: (PrimMonad m, Vec v a) => Buffer (PrimState m) a -> v a -> m ()

traverse :: PrimMonad m => Buffer (PrimState m) a -> (a -> m b) -> Buffer (PrimState m) b

traverse_ :: PrimMonad m => Buffer (PrimState m) a -> (a -> m b) -> m ()

freeze :: Buffer (PrimState m) a -> Array a

freezeSlice :: Buffer (PrimState m) a -> Int -> Int -> Array a

shift :: Buffer (PrimState m) a -> Int -> m ()

clear :: Buffer (PrimState m) a -> m ()

pop :: Buffer (PrimState m) a -> m a

popList :: Buffer (PrimState m) a -> Int -> m [a]
popVec :: Buffer (PrimState m) a -> Int -> m [a]

