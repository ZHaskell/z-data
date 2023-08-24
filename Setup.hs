{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Monad
import Distribution.Pretty
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.System
import Distribution.Utils.NubList
import Distribution.Types.BuildInfo
import Distribution.Types.HookedBuildInfo
import Distribution.Types.Library
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageDescription
import Data.Maybe
import Data.List
import System.Directory
import System.FilePath
import qualified System.Environment as System

main :: IO ()
main = do
    defaultMainWithHooks simpleUserHooks {
        preConf = zPreConf,
        confHook = \ pd flags -> do
            lbi <- confHook simpleUserHooks pd flags
            baseDir <- getCurrentDirectory
            let verbosity = fromFlag (configVerbosity flags)
                configFile = baseDir </> "third_party" </> "botan" </> "configure.py"
                configFolder = baseDir </> "third_party" </> "botan"
                headerFile = baseDir </> "third_party" </> "botan" </> "botan_all.h"
                placeholder = "build" </> "include" </> "external" </> "cabal.placeholder"
            headerFileExists <- doesFileExist headerFile
            unless headerFileExists $ do
                confExists <- doesFileExist configFile
                if confExists
                then do
                    runConfigureScript configFolder configFile verbosity flags lbi
                    runTouch configFolder placeholder verbosity flags
                else die' verbosity "botan configure script not found."

            return lbi
    ,   regHook = \ _ _ _ _ -> return ()
    }

zPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo
zPreConf args flags = do
    preConf simpleUserHooks args flags

runConfigureScript :: FilePath -> FilePath -> Verbosity -> ConfigFlags -> LocalBuildInfo -> IO ()
runConfigureScript configFolder configFile verbosity flags lbi = do
    env <- System.getEnvironment
    configureFile <- makeAbsolute configFile

    let extraPath = fromNubList $ configProgramPathExtra flags
        spSep = [searchPathSeparator]
        pathEnv = maybe (intercalate spSep extraPath)
                ((intercalate spSep extraPath ++ spSep)++) $ lookup "PATH" env
        pyProgs = simpleProgram <$> ["python", "python2", "python3"]
        progDb = modifyProgramSearchPath
            (\p -> map ProgramSearchPathDir extraPath ++ p) emptyProgramDb
        overEnv = [("PATH", Just pathEnv) | not (null extraPath)]
        hp@(Platform  arch os) = hostPlatform lbi
        -- use gcc/mingw bunlded with GHC
        osStr = if os == Windows then "mingw" else (show (pretty os))
        hostFlag = [ "--cpu=" ++ show (pretty arch), "--os=" ++ osStr]
        -- pass amalgamation to produce botan_all.cpp
        args = configureFile:"--amalgamation":"--disable-shared":hostFlag

    pyConfiguredProg <- forM pyProgs $ \ pyProg ->
        lookupProgram pyProg <$> configureProgram verbosity pyProg progDb

    case msum (pyConfiguredProg) of
      Just py -> runProgramInvocation verbosity $
                 (programInvocation (py {programOverrideEnv = overEnv}) args)
                 { progInvokeCwd = Just configFolder }
      Nothing -> die' verbosity notFoundMsg
  where
      notFoundMsg = "The package's dep(botan) has a 'configure.py' script. "
               ++ "This requires python is discoverable in your path."


runTouch :: FilePath -> FilePath -> Verbosity -> ConfigFlags -> IO ()
runTouch configFolder placeholder verbosity flags = do
    let extraPath = fromNubList $ configProgramPathExtra flags
        progDb = modifyProgramSearchPath
            (\p -> map ProgramSearchPathDir extraPath ++ p) emptyProgramDb
        touchPG = simpleProgram "touch"
    touch <- lookupProgram touchPG <$> configureProgram verbosity touchPG progDb
    case touch of
        Just touch' ->
            runProgramInvocation verbosity $
                 (programInvocation touch' [placeholder])
                 { progInvokeCwd = Just configFolder }
        Nothing -> warn verbosity notFoundMsg
  where
      notFoundMsg = "touch is required to keep a cabal placeholder file, otherwise cabal sdist may produce an unbuildable tarball"
