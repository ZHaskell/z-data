import Prelude hiding ((++), map, replicate, all, init, splitAt, null)
import GHC.Exts (IsList(..))
import Control.DeepSeq

data XList = {-# UNPACK #-} !X :+ XList | XEnd deriving (Show, Eq, Ord)

instance NFData XList where
  rnf (_:+xs) = rnf xs
  rnf _ = ()

infixr 5 :+

instance IsList XList where
    type Item XList = X
    {-# INLINE fromList #-}
    fromList = foldr (:+) XEnd

    {-# INLINE toList #-}
    toList XEnd = []
    toList (x :+ xs) = x : toList xs

(++) :: XList -> XList -> XList
{-# INLINABLE (++) #-}
(++) (x:+xs) ys = x :+ xs ++ ys
(++) _       ys = ys

map :: (X -> X) -> XList -> XList
{-# INLINABLE map #-}
map f (x:+xs) = f x :+ map f xs
map _ _ = XEnd

null :: XList -> Bool
{-# INLINABLE null #-}
null XEnd = True
null _    = False

replicate :: Int -> X -> XList
{-# INLINABLE replicate #-}
replicate 0 _ = XEnd
replicate n x = x :+ replicate (n-1) x

all :: (X -> Bool) -> XList -> Bool
{-# INLINABLE all #-}
all _ XEnd      = True
all p (x :+ xs) = p x && all p xs

init :: XList -> XList
{-# INLINABLE init #-}
init (_:+XEnd) = XEnd
init (x:+xs) = x :+ init xs
init _ = error "empty XList"

splitAt :: Int -> XList -> (XList, XList)
{-# INLINABLE splitAt #-}
splitAt n ls
  | n <= 0     = (XEnd, ls)
  | otherwise  = go n ls
  where
    go :: Int -> XList -> (XList, XList)
    go _  XEnd    = (XEnd, XEnd)
    go 1  (x:+xs) = (x:+XEnd, xs)
    go m  (x:+xs) = (x:+xs', xs'')
        where
        (xs', xs'') = go (m - 1) xs