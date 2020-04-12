{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.CodeGen.ImpGen.Kernels.SegMap
  ( compileSegMap,
  )
where

import Control.Monad.Except
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.Representation.ExplicitMemory
import Futhark.Util.IntegralExp (quotRoundingUp)
import Prelude hiding (quot, rem)

-- | Compile 'SegMap' instance code.
compileSegMap ::
  Pattern ExplicitMemory ->
  SegLevel ->
  SegSpace ->
  KernelBody ExplicitMemory ->
  CallKernelGen ()
compileSegMap pat lvl space kbody = do
  let (is, dims) = unzip $ unSegSpace space
  dims' <- mapM toExp dims
  num_groups' <- traverse toExp $ segNumGroups lvl
  group_size' <- traverse toExp $ segGroupSize lvl
  case lvl of
    SegThread {} -> do
      emit $ Imp.DebugPrint "\n# SegMap" Nothing
      let virt_num_groups = product dims' `quotRoundingUp` unCount group_size'
      sKernelThread "segmap" num_groups' group_size' (segFlat space) $ \constants ->
        virtualiseGroups constants (segVirt lvl) virt_num_groups $ \group_id -> do
          let global_tid =
                Imp.vi32 group_id * unCount group_size'
                  + kernelLocalThreadId constants
          zipWithM_ dPrimV_ is $ unflattenIndex dims' global_tid
          sWhen (isActive $ unSegSpace space)
            $ compileStms mempty (kernelBodyStms kbody)
            $ zipWithM_ (compileThreadResult space constants) (patternElements pat)
            $ kernelBodyResult kbody
    SegGroup {} ->
      sKernelGroup "segmap_intragroup" num_groups' group_size' (segFlat space) $ \constants -> do
        let virt_num_groups = product dims'
        virtualiseGroups constants (segVirt lvl) virt_num_groups $ \group_id -> do
          zipWithM_ dPrimV_ is $ unflattenIndex dims' $ Imp.vi32 group_id
          compileStms mempty (kernelBodyStms kbody)
            $ zipWithM_ (compileGroupResult space constants) (patternElements pat)
            $ kernelBodyResult kbody
