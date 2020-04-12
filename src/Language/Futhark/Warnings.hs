module Language.Futhark.Warnings
  ( Warnings,
    singleWarning,
  )
where

import Data.List (intercalate, sortOn)
import Data.Loc
import Data.Monoid
import Language.Futhark.Core (locStr)
import Prelude

-- | The warnings produced by the compiler.  The 'Show' instance
-- produces a human-readable description.
newtype Warnings = Warnings [(SrcLoc, String)] deriving (Eq)

instance Semigroup Warnings where
  Warnings ws1 <> Warnings ws2 = Warnings $ ws1 <> ws2

instance Monoid Warnings where
  mempty = Warnings mempty

instance Show Warnings where
  show (Warnings []) = ""
  show (Warnings ws) =
    intercalate "\n\n" ws' ++ "\n"
    where
      ws' = map showWarning $ sortOn (off . locOf . fst) ws
      off NoLoc = 0
      off (Loc p _) = posCoff p
      showWarning (loc, w) =
        "Warning at " ++ locStr loc ++ ":\n"
          ++ intercalate "\n" (map ("  " <>) $ lines w)

singleWarning :: SrcLoc -> String -> Warnings
singleWarning loc problem = Warnings [(loc, problem)]
