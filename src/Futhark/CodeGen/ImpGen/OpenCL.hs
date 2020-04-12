module Futhark.CodeGen.ImpGen.OpenCL
  ( compileProg,
  )
where

import qualified Futhark.CodeGen.ImpCode.OpenCL as OpenCL
import qualified Futhark.CodeGen.ImpGen.Kernels as ImpGenKernels
import Futhark.CodeGen.ImpGen.Kernels.ToOpenCL
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m OpenCL.Program
compileProg prog = kernelsToOpenCL <$> ImpGenKernels.compileProg prog
