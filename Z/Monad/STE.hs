module Z.Monad.STE where

import Control.Monad
import Control.Monad.Primitive
import GHC.ST
import GHC.IO
import GHC.Exts
import GHC.Exception

-- | 'ST' Monad with 'Exception's .
--
-- Sometime it's useful to have early exit from pure computation, taking parsing as an example: 
-- we can make every step return a 'Maybe', and check result before continue, but doing this will
-- lead to low efficiency due to extra allocation and pointer checking. 'STE' solves this:
--
-- @
-- newtype MyError = MyError { myErrorMessaeg :: String } deriving Show
-- instance Exception MyError
--
--  f :: Either MyError Int
--  f = runSTE $ do
--      ...
--      throwSTE (MyError "error!")
--      ...
-- @
--
-- No care required before or after 'throwSTE' at all, GHC runtime will take care of this
-- early exit, you can also use 'catchSTE' to handle early exit and resume.
--
newtype STE s e a = STE { unSTE :: STRep s a }

instance Functor (STE s e) where
    {-# INLINE fmap #-}
    fmap f (STE m) = STE $ \ s ->
      case (m s) of { (# new_s, r #) ->
      (# new_s, f r #) }

instance Applicative (STE s e) where
    {-# INLINE pure #-}
    {-# INLINE (*>)   #-}
    {-# INLINE (<*>)   #-}
    pure x = STE (\ s -> (# s, x #))
    m *> k = m >>= \ _ -> k
    (<*>) = ap

instance Monad (STE s e) where
    {-# INLINE (>>=)  #-}
    (>>) = (*>)
    (STE m) >>= k
      = STE (\ s ->
        case (m s) of { (# new_s, r #) ->
        case (k r) of { STE k2 ->
        (k2 new_s) }})

instance PrimMonad (STE s e) where
    type PrimState (STE s e) = s
    primitive = STE

-- | Turn a pure 'ST' computation into 'STE'.
stToSTE :: ST s a -> STE s e a
{-# INLINE stToSTE #-}
stToSTE (ST st) = STE st

-- | Instantiated STE state token to 'RealWorld'.
--
-- We will not be able to turn 'STE' computation into pure 'ST s' again, and exception throw by
-- 'throwSTE' will be thrown like @RealWorld@ exception.
steToIO :: Exception e => STE RealWorld e a -> IO a
{-# INLINE steToIO #-}
steToIO (STE ste) = IO ste

-- | Safely turn 'IO' into @STE RealWorld@ which can't be run with 'runSTE' again.
ioToSTE :: IO a -> STE RealWorld e a 
{-# INLINE ioToSTE #-}
ioToSTE (IO io) = STE io

-- | Run STE computaion to get either an early exit value or final result.
--
-- Note this function only works with @forall s. STE s e a@. Once the state token is instantiated to
-- concrete type such as 'RealWorld', this function will not type check again. This is how we prevent
-- arbitrary exception mixed into @STE s e@ monad.
runSTE :: (forall s. STE s e a) -> Either e a
{-# INLINE runSTE #-}
runSTE (STE ste) = 
    -- there's no async exception allowed here
    -- and we can't really block on anything so it's fine
    case runRW# (maskUninterruptible# (
        \ s0 -> catch#
            (\ s -> case ste s of (# s', r #) -> (# s', Right r #))
            (\ e s -> (# s, Left e #))
            (noDuplicate# s0))) of
        (# _, r #) -> r

-- | Throw a value with type e to early exit 'STE' thread.
throwSTE :: Exception e => e -> (STE s e a)
{-# INLINE throwSTE #-}
throwSTE e = STE (\ s# -> unsafeCoerce# (raiseIO# (toException e) (unsafeCoerce# s#)))

-- | Catch an early exit value with type e and resume computation with a handle function.
catchSTE :: Exception e => STE s e a -> (e -> STE s e a) ->  STE s e a
{-# INLINE catchSTE #-}
catchSTE (STE ste) handle =
    STE (unsafeCoerce# (catch#
        (unsafeCoerce# ste)
        (\ e -> case fromException e of
            Just e' ->  (\ s# -> unsafeCoerce# (unSTE (handle e') (unsafeCoerce# s#)))
            _ -> raiseIO# e)))
