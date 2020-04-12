module Futhark.CodeGen.Backends.SequentialPython
  ( compileProg,
  )
where

import Control.Monad
import qualified Futhark.CodeGen.Backends.GenericPython as GenericPython
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.Backends.GenericPython.Definitions
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory

compileProg ::
  MonadFreshNames m =>
  Maybe String ->
  Prog ExplicitMemory ->
  m String
compileProg module_name =
  ImpGen.compileProg
    >=> GenericPython.compileProg
      module_name
      GenericPython.emptyConstructor
      imports
      defines
      operations
      ()
      []
      []
  where
    imports =
      [ Import "sys" Nothing,
        Import "numpy" $ Just "np",
        Import "ctypes" $ Just "ct",
        Import "time" Nothing
      ]
    defines = [Escape pyValues, Escape pyFunctions, Escape pyPanic, Escape pyTuning]
    operations :: GenericPython.Operations Imp.Sequential ()
    operations =
      GenericPython.defaultOperations
        { GenericPython.opsCompiler = const $ return (),
          GenericPython.opsCopy = copySequentialMemory
        }

copySequentialMemory :: GenericPython.Copy Imp.Sequential ()
copySequentialMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes _bt =
  GenericPython.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copySequentialMemory _ _ destspace _ _ srcspace _ _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace
