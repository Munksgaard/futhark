{-# LANGUAGE FlexibleContexts #-}

module Futhark.CLI.CSOpenCL
  ( main,
  )
where

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import qualified Futhark.CodeGen.Backends.CSOpenCL as CSOpenCL
import Futhark.Compiler.CLI
import Futhark.Passes
import Futhark.Pipeline
import Futhark.Util
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile OpenCL C#"
  "Generate OpenCL C# code from optimised Futhark program."
  gpuPipeline
  $ \() mode outpath prog -> do
    mono_libs <- liftIO $ fromMaybe "." <$> lookupEnv "MONO_PATH"
    let class_name =
          case mode of
            ToLibrary -> Just $ takeBaseName outpath
            ToExecutable -> Nothing
    csprog <- CSOpenCL.compileProg class_name prog
    let cspath = outpath `addExtension` "cs"
    liftIO $ writeFile cspath csprog
    case mode of
      ToLibrary -> return ()
      ToExecutable -> do
        ret <-
          liftIO $
            runProgramWithExitCode
              "csc"
              ["-out:" ++ outpath, "-lib:" ++ mono_libs, "-r:Cloo.clSharp.dll,Mono.Options.dll", cspath, "/unsafe"]
              mempty
        case ret of
          Left err ->
            externalErrorS $ "Failed to run csc: " ++ show err
          Right (ExitFailure code, cscwarn, cscerr) ->
            externalErrorS $ "csc failed with code " ++ show code ++ ":\n" ++ cscerr ++ cscwarn
          Right (ExitSuccess, _, _) -> liftIO $ do
            perms <- liftIO $ getPermissions outpath
            setPermissions outpath $ setOwnerExecutable True perms
