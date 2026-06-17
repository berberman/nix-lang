-- | Parse standalone AST fragments for exact-print editing operations.
module Nix.Lang.ExactPrint.Edit.Fragment
  ( parseExprFragment,
    parseBindingFragment,
    parseAttrKeyFragment,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.ExactPrint.Edit.Types
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Parser (Parser, attrKey, located, nixBinding, nixExpr, runNixParser)
import Nix.Lang.Span
import Text.Megaparsec (eof, errorBundlePretty)

-- | Parse a standalone expression fragment.
parseExprFragment :: Text -> EditResult LExpr
parseExprFragment = parseFragment "<expr>" locatedExprParser ParseExprError
  where
    locatedExprParser = do
      L l expr <- located nixExpr
      pure (L l expr)

-- | Parse a standalone binding fragment.
parseBindingFragment :: Text -> EditResult LBinding
parseBindingFragment = parseFragment "<binding>" locatedBindingParser ParseBindingError
  where
    locatedBindingParser = do
      L l binding <- located nixBinding
      pure (L l binding)

-- | Parse a standalone attribute-key fragment.
parseAttrKeyFragment :: Text -> EditResult LAttrKey
parseAttrKeyFragment = parseFragment "<attr-key>" locatedKeyParser ParseAttrKeyError
  where
    locatedKeyParser = do
      L l key <- located attrKey
      pure (L l key)

-- | Parse a fragment and convert parser failures into 'EditError'.
parseFragment :: String -> Parser a -> (Text -> EditError) -> Text -> EditResult a
parseFragment label parser mkErr src =
  case runNixParser (parser <* eof) label src of
    (Right value, _) -> Right value
    (Left err, _) -> Left (mkErr (T.pack (errorBundlePretty err)))
