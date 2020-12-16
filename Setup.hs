import           Distribution.Simple
import           System.Environment

main = do
  args <- getArgs
  if head args == "configure"
     then defaultMainArgs $ [ "--ghc-options", "-optcxx-std=c++11"
                            ] ++ args
     else defaultMain