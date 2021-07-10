module Z.Data.Vector.Shuffle (
    shuffle
) where

import Z.Data.Vector           ( Vec(toArr), length )

import Z.Data.Vector.Base      ( Vec(fromArr) )
import System.Random.Stateful  ( UniformRange(uniformRM), StatefulGen )  
import Z.Data.Array            ( Arr( writeArr, MArr, thawArr, freezeArr, readArr ) )
import Control.Monad.Primitive ( PrimMonad(PrimState) )
import Prelude hiding          ( length )



-- A shuffle function implements Fisher Yates algorithms. 
shuffle :: (StatefulGen g m, PrimMonad m, PrimState m ~ s, Vec v a) => v a -> g -> m (v a)
shuffle v g = do
    let n = length v
    let (arr, x, y) = toArr v
    w <- thawArr arr x y
    shuffle' (n - 1) w g
    w' <- freezeArr w 0 n 
    return (fromArr w' 0 n)


shuffle' :: (StatefulGen g m, PrimMonad m, PrimState m ~ s, Arr v a) => Int -> MArr v s a -> g -> m ()
shuffle' (-1) _ _ = return ()  
shuffle' n v g = do 
    x <- uniformRM (0, n) g
    y <- readArr v x
    z <- readArr v n
    writeArr v x z 
    writeArr v n y 
    shuffle' (n - 1) v g




