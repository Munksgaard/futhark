{-# LANGUAGE FlexibleContexts #-}

module Futhark.CLI.PyOpenCL
  ( main,
  )
where

import Control.Monad.IO.Class
import qualified Futhark.CodeGen.Backends.PyOpenCL as PyOpenCL
import Futhark.Compiler.CLI
import Futhark.Passes
import System.Directory
import System.FilePath

main :: String -> [String] -> IO ()
main = compilerMain
  ()
  []
  "Compile PyOpenCL"
  "Generate Python + OpenCL code from optimised Futhark program."
  gpuPipeline
  $ \() mode outpath prog -> do
    let class_name =
          case mode of
            ToLibrary -> Just $ takeBaseName outpath
            ToExecutable -> Nothing
    pyprog <- PyOpenCL.compileProg class_name prog
    case mode of
      ToLibrary ->
        liftIO $ writeFile (outpath `addExtension` "py") pyprog
      ToExecutable -> liftIO $ do
        writeFile outpath pyprog
        perms <- liftIO $ getPermissions outpath
        setPermissions outpath $ setOwnerExecutable True perms
