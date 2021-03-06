{-# LANGUAGE FlexibleContexts #-}
module Futhark.CLI.Python (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Directory

import Futhark.Passes
import qualified Futhark.CodeGen.Backends.SequentialPython as SequentialPy
import Futhark.Compiler.CLI

main :: String -> [String] -> IO ()
main = compilerMain () []
       "Compile sequential Python" "Generate sequential Python code from optimised Futhark program."
       sequentialCpuPipeline $ \() mode outpath prog -> do
          let class_name =
                case mode of ToLibrary -> Just $ takeBaseName outpath
                             ToExecutable -> Nothing
          pyprog <- SequentialPy.compileProg class_name prog

          case mode of
            ToLibrary ->
              liftIO $ writeFile (outpath `addExtension` "py") pyprog
            ToExecutable -> liftIO $ do
              writeFile outpath pyprog
              perms <- liftIO $ getPermissions outpath
              setPermissions outpath $ setOwnerExecutable True perms
