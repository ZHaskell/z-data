{-# LANGUAGE CPP #-}
import           Distribution.Simple
import           System.Environment

main = do
#if __GLASGOW_HASKELL__ < 810
  defaultMain
#else
  args <- getArgs
  if head args == "configure"
     then defaultMainArgs $ [ "--ghc-options", "-optcxx-std=c++11"
                            ] ++ args
     else defaultMain
#endif