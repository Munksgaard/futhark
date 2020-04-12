{-# LANGUAGE FlexibleContexts #-}

module Futhark.CLI.OpenCL
  ( main,
  )
where

import Control.Monad.IO.Class
import qualified Futhark.CodeGen.Backends.COpenCL as COpenCL
import Futhark.Compiler.CLI
import Futhark.Passes
import Futhark.Pipeline
import Futhark.Util
import System.Exit
import System.FilePath
import qualified System.Info

main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile OpenCL"
  "Generate OpenCL/C code from optimised Futhark program."
  gpuPipeline
  $ \() mode outpath prog -> do
    cprog <- COpenCL.compileProg prog
    let cpath = outpath `addExtension` "c"
        hpath = outpath `addExtension` "h"
        extra_options
          | System.Info.os == "darwin" =
            ["-framework", "OpenCL"]
          | System.Info.os == "mingw32" =
            ["-lOpenCL64"]
          | otherwise =
            ["-lOpenCL"]
    case mode of
      ToLibrary -> do
        let (header, impl) = COpenCL.asLibrary cprog
        liftIO $ writeFile hpath header
        liftIO $ writeFile cpath impl
      ToExecutable -> do
        liftIO $ writeFile cpath $ COpenCL.asExecutable cprog
        ret <-
          liftIO $
            runProgramWithExitCode
              "gcc"
              ([cpath, "-O", "-std=c99", "-lm", "-o", outpath] ++ extra_options)
              mempty
        case ret of
          Left err ->
            externalErrorS $ "Failed to run gcc: " ++ show err
          Right (ExitFailure code, _, gccerr) ->
            externalErrorS $
              "gcc failed with code "
                ++ show code
                ++ ":\n"
                ++ gccerr
          Right (ExitSuccess, _, _) ->
            return ()
