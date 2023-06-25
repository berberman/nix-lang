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
  prettyNix (NixApp _ (L _ f) (L _ x)) = hsep [prettyNix f, prettyNix x]
  prettyNix (NixBinApp _ op (L _ x) (L _ y)) =
    hsep
      [ prettyNix x,
        pretty $
          showBinOP op,
        prettyNix y
      ]
  prettyNix (NixHasAttr _ (L _ x) (L _ p)) = hsep [prettyNix x, "?", prettyNix p]
  prettyNix (NixPath _ (L _ p)) = prettyNix p
  prettyNix (NixEnvPath _ (L _ p)) = "<" <> pretty p <> ">"
  prettyNix (NixLam _ (L _ pat) (L _ x)) = nestedSep [prettyNix pat <> ":", prettyNix x]
  prettyNix (NixList _ xs) = case xs of
    [] -> "[]"
    _ -> nestedSep $ "[" : (prettyNix . unLoc <$> xs) <> ["]"]
  prettyNix (NixSet _ NixSetRecursive (L _ bindings)) = case bindings of
    [] -> "rec {}"
    xs -> nestedSep $ "rec {" : (prettyNix . unLoc <$> xs) <> ["}"]
  prettyNix (NixSet _ NixSetNonRecursive (L _ bindings)) = case bindings of
    [] -> "{}"
    xs -> nestedSep $ "{" : (prettyNix . unLoc <$> xs) <> ["}"]
  prettyNix (NixSelect _ (L _ x) (L _ p) mx) = prettyNix x <> "." <> prettyNix p <> alt
    where
      alt = maybe mempty ((" or " <>) . prettyNix . unLoc) mx
  prettyNix (NixLet _ (L _ bindings) (L _ x)) = group $ vsep ["let", indent 2 (vsep $ prettyNix . unLoc <$> bindings), "in " <> prettyNix x]
  prettyNix _ = undefined

nestedSep :: [Doc ann] -> Doc ann
nestedSep = group . nest 2 . vsep

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

instance PrettyNixId p => PrettyNix (NixPath p) where
  prettyNix (NixLiteralPath _ p) = pretty p
  prettyNix (NixInterpolPath _ parts) = hcat $ prettyStringPart id . unLoc <$> parts
  prettyNix _ = extErr

instance PrettyNixId p => PrettyNix (NixBinding p) where
  prettyNix (NixNormalBinding _ (L _ p) (L _ x)) = hsep [prettyNix p, "=", prettyNix x] <> ";"
  prettyNix (NixInheritBinding _ mScope names) = "inherit" <+> prettyScope <> prettyNames <> ";"
    where
      prettyScope = maybe mempty (parens . prettyNix . unLoc) mScope
      prettyNames = align $ fillSep $ prettyNix . unLoc <$> names
  prettyNix _ = extErr

instance PrettyNixId p => PrettyNix (NixSetPatBinding p) where
  prettyNix (NixSetPatBinding {..}) = prettyNixId (Proxy @p) (unLoc nspbVar) <> prettyDefault
    where
      prettyDefault = maybe mempty ((" ? " <>) . prettyNix . unLoc) nspbDefault

instance PrettyNixId p => PrettyNix (NixSetPatAs p) where
  prettyNix NixSetPatAs {..} = case nspaLocation of
    NixSetPatAsLeading -> prettyId <> "@"
    NixSetPatAsTrailing -> "@" <> prettyId
    where
      prettyId = prettyNixId (Proxy @p) $ unLoc nspaVar

instance PrettyNixId p => PrettyNix (NixFuncPat p) where
  prettyNix (NixVarPat _ (L _ x)) = prettyNixId (Proxy @p) x
  prettyNix (NixSetPat _ ellipses mAs params) = case mAs of
    (Just (L _ NixSetPatAs {nspaLocation = NixSetPatAsLeading})) -> prettyAs <> prettyParams
    (Just (L _ NixSetPatAs {nspaLocation = NixSetPatAsTrailing})) -> prettyParams <> prettyAs
    Nothing -> prettyParams
    where
      prettyAs = maybe mempty (prettyNix . unLoc) mAs
      prettyParams = encloseSep "{ " " }" ", " ((prettyNix . unLoc <$> params) <> ["..." | ellipses == NixSetPatIsEllipses])
  prettyNix _ = extErr
