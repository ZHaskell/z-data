{-# LANGUAGE CPP #-}
import           Distribution.Simple
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.Simple.Program
import           System.Environment

main = do
#ifdef darwin_HOST_OS
    mainArgs <- getArgs
    if head mainArgs == "build"
    then defaultMainWithHooksArgs simpleUserHooks {
            preBuild = \ a b -> getSysroot a b >> preBuild simpleUserHooks a b
#if __GLASGOW_HASKELL__ < 810
        } mainArgs
#else
        } $ if cabalVersion <= mkVersion [3,2,0]
            then ("--ghc-options":"-optcxx-std=c++11":mainArgs)
            else mainArgs
#endif
    else defaultMain

#else
    defaultMain
#endif

getSysroot :: Args -> BuildFlags -> IO ()
getSysroot _ flags = do
    let verbosity = fromFlag $ buildVerbosity flags
    sysroot <- getProgramInvocationOutput verbosity (simpleProgramInvocation "xcrun" ["--show-sdk-path"])
    let sysroot' = head (lines sysroot) ++ "/usr/include"
    notice verbosity ("Use sysroot include at: " ++ sysroot')
    cflags <- lookupEnv "CFLAGS" >>= return . maybe "" id
    setEnv "CFLAGS" $ "-isystem " ++ sysroot' ++ (' ' : cflags)
    cxxflags <- lookupEnv "CXXFLAGS" >>= return . maybe "" id
    setEnv "CXXFLAGS" $ "-isystem " ++ sysroot' ++ (' ' : cxxflags)
