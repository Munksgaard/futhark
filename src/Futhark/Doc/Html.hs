module Futhark.Doc.Html
  ( primTypeHtml,
    prettyU,
    renderName,
    joinBy,
    commas,
    brackets,
    braces,
    parens,
    pipes,
  )
where

import Futhark.Util.Pretty (Doc, ppr)
import Language.Futhark
import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.PrettyPrint.Mainland as PP (pretty)

docToHtml :: Doc -> Html
docToHtml = toHtml . PP.pretty 80

primTypeHtml :: PrimType -> Html
primTypeHtml = docToHtml . ppr

prettyU :: Uniqueness -> Html
prettyU = docToHtml . ppr

renderName :: Name -> Html
renderName name = docToHtml (ppr name)

joinBy :: Html -> [Html] -> Html
joinBy _ [] = mempty
joinBy _ [x] = x
joinBy sep (x : xs) = x <> foldMap (sep <>) xs

commas :: [Html] -> Html
commas = joinBy (toHtml ", ")

parens :: Html -> Html
parens x = toHtml "(" <> x <> toHtml ")"

braces :: Html -> Html
braces x = toHtml "{" <> x <> toHtml "}"

brackets :: Html -> Html
brackets x = toHtml "[" <> x <> toHtml "]"

pipes :: [Html] -> Html
pipes = joinBy (toHtml " | ")
