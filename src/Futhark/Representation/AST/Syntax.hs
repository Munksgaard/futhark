{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Futhark core language skeleton.  Concrete representations further
-- extend this skeleton by defining a "lore", which specifies concrete
-- annotations ("Futhark.Representation.AST.Annotations") and
-- semantics.
module Futhark.Representation.AST.Syntax
  ( module Language.Futhark.Core,
    module Futhark.Representation.AST.Annotations,
    module Futhark.Representation.AST.Syntax.Core,

    -- * Types
    Uniqueness (..),
    NoUniqueness (..),
    Rank (..),
    ArrayShape (..),
    Space (..),
    TypeBase (..),
    Diet (..),

    -- * Abstract syntax tree
    Ident (..),
    SubExp (..),
    PatElem,
    PatElemT (..),
    PatternT (..),
    Pattern,
    StmAux (..),
    Stm (..),
    Stms,
    Result,
    BodyT (..),
    Body,
    BasicOp (..),
    UnOp (..),
    BinOp (..),
    CmpOp (..),
    ConvOp (..),
    DimChange (..),
    ShapeChange,
    ExpT (..),
    Exp,
    LoopForm (..),
    IfAttr (..),
    IfSort (..),
    ConstFun (..),
    Safety (..),
    LambdaT (..),
    Lambda,

    -- * Definitions
    Param (..),
    FParam,
    LParam,
    FunDef (..),
    EntryPoint,
    EntryPointType (..),
    Prog (..),

    -- * Utils
    oneStm,
    stmsFromList,
    stmsToList,
    stmsHead,
  )
where

import Data.Foldable
import Data.Loc
import qualified Data.Sequence as Seq
import Futhark.Representation.AST.Annotations
import Futhark.Representation.AST.Syntax.Core
import Language.Futhark.Core

-- | A type alias for namespace control.
type PatElem lore = PatElemT (LetAttr lore)

-- | A pattern is conceptually just a list of names and their types.
data PatternT attr
  = Pattern
      { -- | existential context (sizes and memory blocks)
        patternContextElements :: [PatElemT attr],
        -- | "real" values
        patternValueElements :: [PatElemT attr]
      }
  deriving (Ord, Show, Eq)

instance Functor PatternT where
  fmap f (Pattern ctx val) = Pattern (map (fmap f) ctx) (map (fmap f) val)

instance Semigroup (PatternT attr) where
  Pattern cs1 vs1 <> Pattern cs2 vs2 = Pattern (cs1 ++ cs2) (vs1 ++ vs2)

instance Monoid (PatternT attr) where
  mempty = Pattern [] []

-- | A type alias for namespace control.
type Pattern lore = PatternT (LetAttr lore)

-- | Auxilliary Information associated with a statement.
data StmAux attr
  = StmAux
      { stmAuxCerts :: !Certificates,
        stmAuxAttr :: attr
      }
  deriving (Ord, Show, Eq)

-- | A local variable binding.
data Stm lore
  = Let
      { stmPattern :: Pattern lore,
        stmAux :: StmAux (ExpAttr lore),
        stmExp :: Exp lore
      }

deriving instance Annotations lore => Ord (Stm lore)

deriving instance Annotations lore => Show (Stm lore)

deriving instance Annotations lore => Eq (Stm lore)

-- | A sequence of statements.
type Stms lore = Seq.Seq (Stm lore)

oneStm :: Stm lore -> Stms lore
oneStm = Seq.singleton

stmsFromList :: [Stm lore] -> Stms lore
stmsFromList = Seq.fromList

stmsToList :: Stms lore -> [Stm lore]
stmsToList = toList

stmsHead :: Stms lore -> Maybe (Stm lore, Stms lore)
stmsHead stms = case Seq.viewl stms of
  stm Seq.:< stms' -> Just (stm, stms')
  Seq.EmptyL -> Nothing

-- | The result of a body is a sequence of subexpressions.
type Result = [SubExp]

-- | A body consists of a number of bindings, terminating in a result
-- (essentially a tuple literal).
data BodyT lore
  = Body
      { bodyAttr :: BodyAttr lore,
        bodyStms :: Stms lore,
        bodyResult :: Result
      }

deriving instance Annotations lore => Ord (BodyT lore)

deriving instance Annotations lore => Show (BodyT lore)

deriving instance Annotations lore => Eq (BodyT lore)

-- | Type alias for namespace reasons.
type Body = BodyT

-- | The new dimension in a 'Reshape'-like operation.  This allows us to
-- disambiguate "real" reshapes, that change the actual shape of the
-- array, from type coercions that are just present to make the types
-- work out.  The two constructors are considered equal for purposes of 'Eq'.
data DimChange d
  = -- | The new dimension is guaranteed to be numerically
    -- equal to the old one.
    DimCoercion d
  | -- | The new dimension is not necessarily numerically
    -- equal to the old one.
    DimNew d
  deriving (Ord, Show)

instance Eq d => Eq (DimChange d) where
  DimCoercion x == DimNew y = x == y
  DimCoercion x == DimCoercion y = x == y
  DimNew x == DimCoercion y = x == y
  DimNew x == DimNew y = x == y

instance Functor DimChange where
  fmap f (DimCoercion d) = DimCoercion $ f d
  fmap f (DimNew d) = DimNew $ f d

instance Foldable DimChange where
  foldMap f (DimCoercion d) = f d
  foldMap f (DimNew d) = f d

instance Traversable DimChange where
  traverse f (DimCoercion d) = DimCoercion <$> f d
  traverse f (DimNew d) = DimNew <$> f d

-- | A list of 'DimChange's, indicating the new dimensions of an array.
type ShapeChange d = [DimChange d]

-- | A primitive operation that returns something of known size and
-- does not itself contain any bindings.
data BasicOp lore
  = -- | A variable or constant.
    SubExp SubExp
  | -- | Semantically and operationally just identity, but is
    -- invisible/impenetrable to optimisations (hopefully).  This is
    -- just a hack to avoid optimisation (so, to work around compiler
    -- limitations).
    Opaque SubExp
  | -- | Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
    -- Second arg is the element type of the rows of the array.
    -- Scalar operations
    ArrayLit [SubExp] Type
  | -- | Unary operation.
    UnOp UnOp SubExp
  | -- | Binary operation.
    BinOp BinOp SubExp SubExp
  | -- | Comparison - result type is always boolean.
    CmpOp CmpOp SubExp SubExp
  | -- | Conversion "casting".
    ConvOp ConvOp SubExp
  | -- | Turn a boolean into a certificate, halting the program with the
    -- given error message if the boolean is false.
    Assert SubExp (ErrorMsg SubExp) (SrcLoc, [SrcLoc])
  | -- Primitive array operations

    -- | The certificates for bounds-checking are part of the 'Stm'.
    Index VName (Slice SubExp)
  | -- | An in-place update of the given array at the given position.
    -- Consumes the array.
    Update VName (Slice SubExp) SubExp
  | -- | @concat@0([1],[2, 3, 4]) = [1, 2, 3, 4]@.
    Concat Int VName [VName] SubExp
  | -- | Copy the given array.  The result will not alias anything.
    Copy VName
  | -- | Manifest an array with dimensions represented in the given
    -- order.  The result will not alias anything.
    Manifest [Int] VName
  | -- Array construction.

    -- | @iota(n, x, s) = [x,x+s,..,x+(n-1)*s]@.
    --
    -- The 'IntType' indicates the type of the array returned and the
    -- offset/stride arguments, but not the length argument.
    Iota SubExp SubExp SubExp IntType
  | -- | @replicate([3][2],1) = [[1,1], [1,1], [1,1]]@
    Replicate Shape SubExp
  | -- | Repeat each dimension of the input array some number of times,
    -- given by the corresponding shape.  For an array of rank @k@, the
    -- list must contain @k@ shapes.  A shape may be empty (in which
    -- case the dimension is not repeated, but it is still present).
    -- The last shape indicates the amount of extra innermost
    -- dimensions.  All other extra dimensions are added *before* the original dimension.
    Repeat [Shape] Shape VName
  | -- | Create array of given type and shape, with undefined elements.
    Scratch PrimType [SubExp]
  | -- Array index space transformation.

    -- | 1st arg is the new shape, 2nd arg is the input array *)
    Reshape (ShapeChange SubExp) VName
  | -- | Permute the dimensions of the input array.  The list
    -- of integers is a list of dimensions (0-indexed), which
    -- must be a permutation of @[0,n-1]@, where @n@ is the
    -- number of dimensions in the input array.
    Rearrange [Int] VName
  | -- | Rotate the dimensions of the input array.  The list of
    -- subexpressions specify how much each dimension is rotated.  The
    -- length of this list must be equal to the rank of the array.
    Rotate [SubExp] VName
  deriving (Eq, Ord, Show)

-- | The root Futhark expression type.  The 'Op' constructor contains
-- a lore-specific operation.  Do-loops, branches and function calls
-- are special.  Everything else is a simple 'BasicOp'.
data ExpT lore
  = -- | A simple (non-recursive) operation.
    BasicOp (BasicOp lore)
  | Apply Name [(SubExp, Diet)] [RetType lore] (ConstFun, Safety, SrcLoc, [SrcLoc])
  | If SubExp (BodyT lore) (BodyT lore) (IfAttr (BranchType lore))
  | -- | @loop {a} = {v} (for i < n|while b) do b@.  The merge
    -- parameters are divided into context and value part.
    DoLoop [(FParam lore, SubExp)] [(FParam lore, SubExp)] (LoopForm lore) (BodyT lore)
  | Op (Op lore)

deriving instance Annotations lore => Eq (ExpT lore)

deriving instance Annotations lore => Show (ExpT lore)

deriving instance Annotations lore => Ord (ExpT lore)

-- | Does this function call actually represent a reference to a
-- run-time constant?  This has implications for inlining.
data ConstFun = ConstFun | NotConstFun deriving (Eq, Ord, Show)

-- | Whether something is safe or unsafe (mostly function calls, and
-- in the context of whether operations are dynamically checked).
-- When we inline an 'Unsafe' function, we remove all safety checks in
-- its body.  The 'Ord' instance picks 'Unsafe' as being less than
-- 'Safe'.
data Safety = Unsafe | Safe deriving (Eq, Ord, Show)

-- | For-loop or while-loop?
data LoopForm lore
  = ForLoop VName IntType SubExp [(LParam lore, VName)]
  | WhileLoop VName

deriving instance Annotations lore => Eq (LoopForm lore)

deriving instance Annotations lore => Show (LoopForm lore)

deriving instance Annotations lore => Ord (LoopForm lore)

-- | Data associated with a branch.
data IfAttr rt
  = IfAttr
      { ifReturns :: [rt],
        ifSort :: IfSort
      }
  deriving (Eq, Show, Ord)

data IfSort
  = -- | An ordinary branch.
    IfNormal
  | -- | A branch where the "true" case is what
    -- we are actually interested in, and the
    -- "false" case is only present as a fallback
    -- for when the true case cannot be safely
    -- evaluated.  the compiler is permitted to
    -- optimise away the branch if the true case
    -- contains only safe statements.
    IfFallback
  deriving (Eq, Show, Ord)

-- | A type alias for namespace control.
type Exp = ExpT

-- | Anonymous function for use in a SOAC.
data LambdaT lore
  = Lambda
      { lambdaParams :: [LParam lore],
        lambdaBody :: BodyT lore,
        lambdaReturnType :: [Type]
      }

deriving instance Annotations lore => Eq (LambdaT lore)

deriving instance Annotations lore => Show (LambdaT lore)

deriving instance Annotations lore => Ord (LambdaT lore)

-- | Type alias for namespacing reasons.
type Lambda = LambdaT

type FParam lore = Param (FParamAttr lore)

type LParam lore = Param (LParamAttr lore)

-- | Function Declarations
data FunDef lore
  = FunDef
      { -- | Contains a value if this function is
        -- an entry point.
        funDefEntryPoint :: Maybe EntryPoint,
        funDefName :: Name,
        funDefRetType :: [RetType lore],
        funDefParams :: [FParam lore],
        funDefBody :: BodyT lore
      }

deriving instance Annotations lore => Eq (FunDef lore)

deriving instance Annotations lore => Show (FunDef lore)

deriving instance Annotations lore => Ord (FunDef lore)

-- | Information about the parameters and return value of an entry
-- point.  The first element is for parameters, the second for return
-- value.
type EntryPoint = ([EntryPointType], [EntryPointType])

-- | Every entry point argument and return value has an annotation
-- indicating how it maps to the original source program type.
data EntryPointType
  = -- | Is an unsigned integer or array of unsigned
    -- integers.
    TypeUnsigned
  | -- | A black box type comprising this many core
    -- values.  The string is a human-readable
    -- description with no other semantics.
    TypeOpaque String Int
  | -- | Maps directly.
    TypeDirect
  deriving (Eq, Show, Ord)

-- | An entire Futhark program.
newtype Prog lore = Prog {progFuns :: [FunDef lore]}
  deriving (Eq, Ord, Show)
