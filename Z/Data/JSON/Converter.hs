{-|
Module      : Z.Data.JSON.Converter
Description : IR Converter
Copyright   : (c) Dong Han, 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides tools for converting protocol IR (e.g. 'Value') to Haskell ADTs:

-}

module Z.Data.JSON.Converter where

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Fail             as Fail
import           Control.DeepSeq
import           GHC.Generics
import qualified Z.Data.JSON.Builder            as JB
import qualified Z.Data.Text                    as T
import qualified Z.Data.Text.Print              as T

-- | Run a 'Converter' with input value.
convert :: (a -> Converter r) -> a -> Either ConvertError r
{-# INLINE convert #-}
convert m v = runConverter (m v) (\ paths msg -> (Left (ConvertError paths msg))) Right

-- | Elements of a (JSON) Value path used to describe the location of an error.
data PathElement
    = Key {-# UNPACK #-} !T.Text
        -- ^ Path element of a key into an object,
        -- \"object.key\".
    | Index {-# UNPACK #-} !Int
        -- ^ Path element of an index into an
        -- array, \"array[index]\".
    | Embedded
        -- ^ path of a embedded (JSON) String
  deriving (Eq, Show, Ord, Generic, NFData)

-- | Error info with (JSON) Path info.
data ConvertError = ConvertError { errPath :: [PathElement], errMsg :: T.Text }
        deriving (Eq, Ord, Generic, NFData)

instance Show ConvertError where
    show = T.toString

instance T.Print ConvertError where
    toUTF8BuilderP _ (ConvertError [] msg) = T.toUTF8Builder msg
    toUTF8BuilderP _ (ConvertError paths msg) = do
        mapM_ renderPath (reverse paths)
        T.char7 ':'
        T.toUTF8Builder msg
      where
        renderPath (Index ix) = T.char7 '[' >> T.int ix >> T.char7 ']'
        renderPath (Key k)    = T.char7 '.' >> (JB.string k)
        renderPath Embedded   = "<Embedded>"

-- | 'Converter' provides a monadic interface to convert protocol IR  (e.g.'Value') to Haskell ADT.
--
newtype Converter a = Converter { runConverter :: forall r. ([PathElement] -> T.Text -> r) -> (a -> r) -> r }

instance Functor Converter where
    fmap f m = Converter (\ kf k -> runConverter m kf (k . f))
    {-# INLINE fmap #-}

instance Applicative Converter where
    pure a = Converter (\ _ k -> k a)
    {-# INLINE pure #-}
    (Converter f) <*> (Converter g) = Converter (\ kf k ->
        f kf (\ f' ->  g kf (k . f')))
    {-# INLINE (<*>) #-}

instance Alternative Converter where
    {-# INLINE (<|>) #-}
    (Converter f) <|> (Converter g) = Converter (\ kf k -> f (\ _ _ -> g kf k) k)
    {-# INLINE empty #-}
    empty = fail' "Z.Data.JSON.Converter(Alternative).empty"

instance MonadPlus Converter where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance Monad Converter where
    (Converter f) >>= g = Converter (\ kf k ->
        f kf (\ a -> runConverter (g a) kf k))
    {-# INLINE (>>=) #-}
    return = pure
    {-# INLINE return #-}

instance Fail.MonadFail Converter where
    {-# INLINE fail #-}
    fail = fail' . T.pack

-- | 'T.Text' version of 'fail'.
fail' :: T.Text -> Converter a
{-# INLINE fail' #-}
fail' msg = Converter (\ kf _ -> kf [] msg)

-- | Add (JSON) Path context to a converter
--
-- When converting a complex structure, it helps to annotate (sub)converters
-- with context, so that if an error occurs, you can find its location.
--
-- > withFlatMapR "Person" $ \o ->
-- >   Person
-- >     <$> o .: "name" <?> Key "name"
-- >     <*> o .: "age" <?> Key "age"
--
-- (Standard methods like '(.:)' already do this.)
--
-- With such annotations, if an error occurs, you will get a (JSON) Path location of that error.
(<?>) :: Converter a -> PathElement -> Converter a
{-# INLINE (<?>) #-}
(Converter p) <?> path = Converter (\ kf k -> p (kf . (path:)) k)
infixl 9 <?>

-- | Add context to a failure message, indicating the name of the structure
-- being converted.
--
-- > prependContext "MyType" (fail "[error message]")
-- > -- Error: "converting MyType failed, [error message]"
prependContext :: T.Text -> Converter a -> Converter a
{-# INLINE prependContext #-}
prependContext name (Converter p) = Converter (\ kf k ->
    p (\ paths msg -> kf paths (T.concat ["converting ", name, " failed, ", msg])) k)
