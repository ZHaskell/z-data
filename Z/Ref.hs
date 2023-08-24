module Z.Data.Ref (
    -- * Boxed References
    Ref, newRef, writeRef, readRef, modifyRef, atomicModifyRef,
    -- * Primitive References
    PrimRef(..)
  , Prim(..)
  , newPrimRef
  , readPrimRef
  , writePrimRef
  , modifyPrimRef
    -- * Atomic operations for 'Counter'
  , Counter
    -- ** return value BEFORE atomic operation
  , atomicAddCounter
  , atomicSubCounter
  , atomicAndCounter
  , atomicNandCounter
  , atomicOrCounter
  , atomicXorCounter
    -- ** without returning
  , atomicAddCounter_
  , atomicSubCounter_
  , atomicAndCounter_
  , atomicNandCounter_
  , atomicOrCounter_
  , atomicXorCounter_
    -- * Unlifted References
  , UnliftedRef(..)
  , PrimUnlifted(..)
  , newUnliftedRef
  , readUnliftedRef
  , writeUnliftedRef
  , modifyUnliftedRef
) where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import Z.Data.Ref.PrimRef
import Z.Data.Ref.UnliftedRef

type Ref s a = MutVar s a

newRef :: PrimMonad m => a -> m (Ref (PrimState m) a)
newRef = newMutVar

writeRef :: PrimMonad m => Ref (PrimState m) a -> a -> m ()
writeRef = writeMutVar

readRef :: PrimMonad m => Ref (PrimState m) a -> m a
readRef = readMutVar

modifyRef :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef = modifyMutVar'

atomicModifyRef :: Ref RealWorld a -> (a -> (a, b)) -> IO b
atomicModifyRef = atomicModifyMutVar'
