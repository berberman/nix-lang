-- | Run parser on standalone AST fragments for internal exact-print preparation.
module Nix.Lang.ExactPrint.Internal.Parse
  ( parseExpr,
    parseBinding,
    parseAttrKey,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.ExactPrint.Internal.Types
import Nix.Lang.Parser (Parser, attrKey, located, nixBinding, nixExpr, runNixParser)
import Nix.Lang.Span
import Nix.Lang.Types.Parsed
import Text.Megaparsec (eof, errorBundlePretty)

-- | Parse a standalone expression fragment.
parseExpr :: Text -> EPResult LExpr
parseExpr = parseFragment "<expr>" locatedExprParser ParseExprError
  where
    locatedExprParser = do
      L l expr <- located nixExpr
      pure (L l expr)

-- | Parse a standalone binding fragment.
parseBinding :: Text -> EPResult LBinding
parseBinding = parseFragment "<binding>" locatedBindingParser ParseBindingError
  where
    locatedBindingParser = do
      L l binding <- located nixBinding
      pure (L l binding)

-- | Parse a standalone attribute-key fragment.
parseAttrKey :: Text -> EPResult LAttrKey
parseAttrKey = parseFragment "<attr-key>" locatedKeyParser ParseAttrKeyError
  where
    locatedKeyParser = do
      L l key <- located attrKey
      pure (L l key)

-- | Parse a fragment and convert parser failures into 'EPError'.
parseFragment :: String -> Parser a -> (Text -> EPError) -> Text -> EPResult a
parseFragment label parser mkErr src =
  case runNixParser (parser <* eof) label src of
    (Right value, _) -> Right value
    (Left err, _) -> Left (mkErr (T.pack (errorBundlePretty err)))
