module Nix.Lang.Pretty where

import Data.Data (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Parser (escapedChars)
import Nix.Lang.Types
import Nix.Lang.Utils
import Prettyprinter

class PrettyNix a where
  prettyNix :: a -> Doc ann

class PrettyNixId p where
  prettyNixId :: Proxy p -> NixId p -> Doc ann

instance PrettyNixId Ps where
  prettyNixId _ = pretty

extErr :: a
extErr = error "unimplemented for extension"

instance PrettyNix (NixLit p) where
  prettyNix (NixUri _ uri) = pretty uri
  prettyNix (NixInteger _ int) = pretty int
  prettyNix (NixFloat _ float) = pretty float
  prettyNix (NixBoolean _ bool) = if bool then "true" else "false"
  prettyNix (NixNull _) = "null"
  prettyNix _ = extErr

instance PrettyNixId p => PrettyNix (NixExpr p) where
  prettyNix (NixLit _ (L _ lit)) = prettyNix lit
  prettyNix (NixString _ (L _ str)) = prettyNix str
  prettyNix (NixVar _ (L _ x)) = prettyNixId (Proxy @p) x
  prettyNix (NixPar _ (L _ x)) = parens $ prettyNix x
  prettyNix (NixNegApp _ (L _ x)) = "-" <> prettyNix x
  prettyNix (NixNotApp _ (L _ x)) = "!" <> prettyNix x
  prettyNix _ = undefined

prettyStringPart :: PrettyNixId p => (Text -> Text) -> NixStringPart p -> Doc ann
prettyStringPart escape (NixStringLiteral _ text) = pretty $ escape text
prettyStringPart _ (NixStringInterpol _ expr) = "${" <> prettyNix (unLoc expr) <> "}"
prettyStringPart _ _ = extErr

instance PrettyNixId p => PrettyNix (NixString p) where
  prettyNix (NixDoubleQuotesString _ parts) = dquotes $ prettyParts $ fmap unLoc parts
    where
      prettyParts = mconcat . fmap (prettyStringPart escape)
      escape = foldr (.) id [T.replace (T.singleton char) (T.cons '\\' (T.singleton code)) | (code, char) <- escapedChars]
  prettyNix (NixDoubleSingleQuotesString _ parts) = vcat ["''", nest 2 $ prettyParts $ fmap unLoc parts, "''"]
    where
      prettyParts = mconcat . fmap (prettyStringPart escape)
      escape = foldr (.) id [T.replace "${" "''${", T.replace "''" "'''"]
  prettyNix _ = extErr

instance PrettyNixId p => PrettyNix (NixAttrKey p) where
  prettyNix (NixStaticAttrKey _ (L _ x)) = prettyNixId (Proxy @p) x
  prettyNix (NixDynamicStringAttrKey _ parts) = prettyNix $ NixDoubleQuotesString undefined parts
  prettyNix (NixDynamicInterpolAttrKey _ expr) = "${" <> prettyNix (unLoc expr) <> "}"
  prettyNix _ = extErr

instance PrettyNixId p => PrettyNix (NixAttrPath p) where
  prettyNix (NixAttrPath keys) = hcat . punctuate dot $ prettyNix . unLoc <$> keys
