module Futhark.Pass.FirstOrderTransform
  ( firstOrderTransform,
  )
where

import Futhark.Pass
import Futhark.Representation.Kernels (Kernels)
import Futhark.Representation.SOACS (SOACS)
import Futhark.Transform.FirstOrderTransform (transformFunDef)

firstOrderTransform :: Pass SOACS Kernels
firstOrderTransform =
  Pass
    "first order transform"
    "Transform all second-order array combinators to for-loops."
    $ intraproceduralTransformation transformFunDef
