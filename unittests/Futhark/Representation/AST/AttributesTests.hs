{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Representation.AST.AttributesTests
  ( tests,
  )
where

import qualified Futhark.Representation.AST.Attributes.RearrangeTests
import qualified Futhark.Representation.AST.Attributes.ReshapeTests
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "AttributesTests"
    [ Futhark.Representation.AST.Attributes.ReshapeTests.tests,
      Futhark.Representation.AST.Attributes.RearrangeTests.tests
    ]
