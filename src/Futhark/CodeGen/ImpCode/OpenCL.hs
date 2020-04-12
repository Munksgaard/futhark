-- | Imperative code with an OpenCL component.
--
-- Apart from ordinary imperative code, this also carries around an
-- OpenCL program as a string, as well as a list of kernels defined by
-- the OpenCL program.
--
-- The imperative code has been augmented with a 'LaunchKernel'
-- operation that allows one to execute an OpenCL kernel.
module Futhark.CodeGen.ImpCode.OpenCL
  ( Program (..),
    Function,
    FunctionT (Function),
    Code,
    KernelName,
    KernelArg (..),
    OpenCL (..),
    Safety (..),
    numFailureParams,
    KernelTarget (..),
    FailureMsg (..),
    module Futhark.CodeGen.ImpCode,
    module Futhark.Representation.Kernels.Sizes,
  )
where

import qualified Data.Map as M
import Futhark.CodeGen.ImpCode hiding (Code, Function)
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.Kernels.Sizes
import Futhark.Util.Pretty

-- | An program calling OpenCL kernels.
data Program
  = Program
      { openClProgram :: String,
        -- | Must be prepended to the program.
        openClPrelude :: String,
        openClKernelNames :: M.Map KernelName Safety,
        -- | So we can detect whether the device is capable.
        openClUsedTypes :: [PrimType],
        -- | Runtime-configurable constants.
        openClSizes :: M.Map Name SizeClass,
        -- | Assertion failure error messages.
        openClFailures :: [FailureMsg],
        hostFunctions :: Functions OpenCL
      }

-- | Something that can go wrong in a kernel.  Part of the machinery
-- for reporting error messages from within kernels.
data FailureMsg
  = FailureMsg
      { failureError :: ErrorMsg Exp,
        failureBacktrace :: String
      }

-- | A function calling OpenCL kernels.
type Function = Imp.Function OpenCL

-- | A piece of code calling OpenCL.
type Code = Imp.Code OpenCL

-- | The name of a kernel.
type KernelName = String

-- | An argument to be passed to a kernel.
data KernelArg
  = -- | Pass the value of this scalar expression as argument.
    ValueKArg Exp PrimType
  | -- | Pass this pointer as argument.
    MemKArg VName
  | -- | Create this much local memory per workgroup.
    SharedMemoryKArg (Count Bytes Exp)
  deriving (Show)

-- | Whether a kernel can potentially fail (because it contains bounds
-- checks and such).
data MayFail = MayFail | CannotFail
  deriving (Show)

-- | Information about bounds checks and how sensitive it is to
-- errors.  Ordered by least demanding to most.
data Safety
  = -- | Does not need to know if we are in a failing state, and also
    -- cannot fail.
    SafetyNone
  | -- | Needs to be told if there's a global failure, and that's it,
    -- and cannot fail.
    SafetyCheap
  | -- | Needs all parameters, may fail itself.
    SafetyFull
  deriving (Eq, Ord, Show)

-- | How many leading failure arguments we must pass when launching a
-- kernel with these safety characteristics.
numFailureParams :: Safety -> Int
numFailureParams SafetyNone = 0
numFailureParams SafetyCheap = 1
numFailureParams SafetyFull = 3

-- | Host-level OpenCL operation.
data OpenCL
  = LaunchKernel Safety KernelName [KernelArg] [Exp] [Exp]
  | GetSize VName Name
  | CmpSizeLe VName Name Exp
  | GetSizeMax VName SizeClass
  deriving (Show)

-- | The target platform when compiling imperative code to a 'Program'
data KernelTarget
  = TargetOpenCL
  | TargetCUDA
  deriving (Eq)

instance Pretty OpenCL where
  ppr = text . show
