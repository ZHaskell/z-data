{-# LANGUAGE CPP #-}
import           Distribution.Simple
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.Simple.Program
import           System.Environment
import qualified System.Environment as System

main = do
    mainArgs <- getArgs
    if head mainArgs == "build"
#ifdef darwin_HOST_OS
    then defaultMainWithHooksArgs simpleUserHooks
            { preBuild = \ a b -> getSysroot a b >> preBuild simpleUserHooks a b }
#else
    then defaultMainWithHooksArgs simpleUserHooks
#endif

#if __GLASGOW_HASKELL__ < 810
            mainArgs
#else
            (if cabalVersion <= mkVersion [3,2,0] then ("--ghc-options":"-optcxx-std=c++11":mainArgs) else mainArgs)
#endif

    else defaultMain


getSysroot :: Args -> BuildFlags -> IO ()
getSysroot _ flags = do
    let verbosity = fromFlag $ buildVerbosity flags
    sysroot <- getProgramInvocationOutput verbosity (simpleProgramInvocation "xcrun" ["--show-sdk-path"])
    let sysroot' = head (lines sysroot)
        system = sysroot' ++ "/../../usr/include"
    notice verbosity ("Use sysroot at: " ++ sysroot')
    notice verbosity ("Use system include at: " ++ system)
    cflags <- lookupEnv "CFLAGS" >>= return . maybe "" id
    setEnv "CFLAGS" $ "-isysroot " ++ sysroot' ++ " -isystem " ++ system ++ (' ' : cflags)
    cxxflags <- lookupEnv "CXXFLAGS" >>= return . maybe "" id
    setEnv "CXXFLAGS" $ "-isysroot " ++ sysroot' ++ " -cxx-isystem " ++ system ++ (' ' : cxxflags)
