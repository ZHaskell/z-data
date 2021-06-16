{-# LANGUAGE CPP #-}
import           Distribution.Simple
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.Simple.Program
import           System.Environment
import qualified System.Environment as System
import           Distribution.Verbosity

main = do
#ifdef darwin_HOST_OS
    sysroot <- getProgramInvocationOutput normal (simpleProgramInvocation "xcrun" ["--show-sdk-path"])
    let sysroot' = head (lines sysroot)
        system = sysroot' ++ "/../../usr/include"
#endif
    mainArgs <- getArgs
    if head mainArgs == "build"
    then do
        let mainArgs' =
#ifdef darwin_HOST_OS
                "--ghc-options":("-optcxx-nostdinc++"):
                "--ghc-options":("-optcxx-cxx-isystem"++ system):
                "--ghc-options":("-optcxx-isystem"++ system):mainArgs
#else
                mainArgs
#endif

        let mainArgs'' =
#if __GLASGOW_HASKELL__ < 810
                mainArgs'
#else
                if cabalVersion <= mkVersion [3,2,0]
                then ("--ghc-options":"-optcxx-std=c++11":mainArgs')
                else mainArgs'
#endif

        defaultMainWithHooksArgs simpleUserHooks mainArgs''
    else defaultMain
