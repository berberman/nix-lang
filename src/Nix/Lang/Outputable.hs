module Nix.Lang.Outputable where

import Data.Data (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Parser (escapedChars)
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Types.Parsed
import Nix.Lang.Utils
import Prettyprinter

class Outputable a where
  output :: a -> Doc ann

class OutputableId p where
  outputId :: Proxy p -> NixId p -> Doc ann

instance OutputableId Ps where
  outputId _ = pretty

extErr :: a
extErr = error "unimplemented for extension"

instance Outputable (NixLit p) where
  output (NixUri _ uri) = pretty uri
  output (NixInteger _ int) = pretty int
  output (NixFloat _ float) = pretty float
  output (NixBoolean _ bool) = if bool then "true" else "false"
  output (NixNull _) = "null"
  output (XNixLit _) = extErr

instance (OutputableId p) => Outputable (NixExpr p) where
  output (NixLit _ (L _ lit)) = output lit
  output (NixString _ (L _ str)) = output str
  output (NixVar _ (L _ x)) = outputId (Proxy @p) x
  output (NixPar _ (L _ x)) = parens $ output x
  output (NixNegApp _ (L _ x)) = "-" <> output x
  output (NixNotApp _ (L _ x)) = "!" <> output x
  output (NixApp _ (L _ f) (L _ x)) = hsep [output f, output x]
  output (NixBinApp _ op (L _ x) (L _ y)) =
    hsep
      [ output x,
        pretty $ showBinOP op,
        output y
      ]
  output (NixHasAttr _ (L _ x) (L _ p)) = hsep [output x, "?", output p]
  output (NixPath _ (L _ p)) = output p
  output (NixEnvPath _ (L _ p)) = "<" <> pretty p <> ">"
  output (NixLam _ (L _ pat) (L _ x)) = nestedSep [output pat <> ":", output x]
  output (NixList _ xs) = case xs of
    [] -> "[]"
    _ -> nestedSep $ "[" : (output . unLoc <$> xs) <> ["]"]
  output (NixSet _ NixSetRecursive (L _ bindings)) = case bindings of
    [] -> "rec {}"
    xs -> nestedSep $ "rec {" : (output . unLoc <$> xs) <> ["}"]
  output (NixSet _ NixSetNonRecursive (L _ bindings)) = case bindings of
    [] -> "{}"
    xs -> nestedSep $ "{" : (output . unLoc <$> xs) <> ["}"]
  output (NixSelect _ (L _ x) (L _ p) mx) = output x <> "." <> output p <> alt
    where
      alt = maybe mempty ((" or " <>) . output . unLoc) mx
  output (NixLet _ (L _ bindings) (L _ x)) =
    group $
      vsep ["let", indent 2 (vsep $ output . unLoc <$> bindings), "in " <> output x]
  output (NixIf _ (L _ cond) (L _ t) (L _ f)) = nestedSep ["if " <> output cond, "then " <> output t, "else " <> output f]
  output (NixWith _ (L _ s) (L _ x)) = nestedSep ["with " <> output s <> ";", output x]
  output (NixAssert _ (L _ s) (L _ x)) = nestedSep ["assert " <> output s <> ";", output x]
  output (XNixExpr _) = extErr

nestedSep :: [Doc ann] -> Doc ann
nestedSep = group . nest 2 . vsep

outputStringPart :: (OutputableId p) => (Text -> Text) -> NixStringPart p -> Doc ann
outputStringPart escape (NixStringLiteral _ text) = pretty $ escape text
outputStringPart _ (NixStringInterpol _ expr) = "${" <> output (unLoc expr) <> "}"
outputStringPart _ (XNixStringPart _) = extErr

instance (OutputableId p) => Outputable (NixString p) where
  output (NixDoubleQuotesString _ parts) = dquotes $ outputParts $ fmap unLoc parts
    where
      outputParts = mconcat . fmap (outputStringPart escape)
      escape = foldr (.) id [T.replace (T.singleton char) (T.cons '\\' (T.singleton code)) | (code, char) <- escapedChars]
  output (NixDoubleSingleQuotesString _ parts) = vcat ["''", nest 2 $ outputParts $ fmap unLoc parts, "''"]
    where
      outputParts = mconcat . fmap (outputStringPart escape)
      escape = foldr (.) id [T.replace "${" "''${", T.replace "''" "'''"]
  output (XNixString _) = extErr

instance (OutputableId p) => Outputable (NixAttrKey p) where
  output (NixStaticAttrKey _ (L _ x)) = outputId (Proxy @p) x
  output (NixDynamicStringAttrKey _ parts) = output $ NixDoubleQuotesString undefined parts
  output (NixDynamicInterpolAttrKey _ expr) = "${" <> output (unLoc expr) <> "}"
  output (XNixNixAttrKey _) = extErr

instance (OutputableId p) => Outputable (NixAttrPath p) where
  output (NixAttrPath _ keys) = hcat . punctuate dot $ output . unLoc <$> keys

instance (OutputableId p) => Outputable (NixPath p) where
  output (NixLiteralPath _ p) = pretty p
  output (NixInterpolPath _ parts) = hcat $ outputStringPart id . unLoc <$> parts
  output (XNixPath _) = extErr

instance (OutputableId p) => Outputable (NixBinding p) where
  output (NixNormalBinding _ (L _ p) (L _ x)) = hsep [output p, "=", output x] <> ";"
  output (NixInheritBinding _ mScope names) = "inherit" <> outputScope <> outputNames names <> ";"
    where
      outputScope = maybe mempty ((" " <>) . output . unLoc) mScope
      outputNames [] = mempty
      outputNames ns = " " <> (align . fillSep $ output . unLoc <$> ns)
  output (XNixBinding _) = extErr

instance (OutputableId p) => Outputable (NixSetPatBinding p) where
  output (NixSetPatBinding {..}) = outputId (Proxy @p) (unLoc nspbVar) <> outputDefault
    where
      outputDefault = maybe mempty ((" ? " <>) . output . unLoc) nspbDefault

instance (OutputableId p) => Outputable (NixSetPatAs p) where
  output NixSetPatAs {..} = case nspaLocation of
    NixSetPatAsLeading -> outputNm <> "@"
    NixSetPatAsTrailing -> "@" <> outputNm
    where
      outputNm = outputId (Proxy @p) $ unLoc nspaVar

instance (OutputableId p) => Outputable (NixFuncPat p) where
  output (NixVarPat _ (L _ x)) = outputId (Proxy @p) x
  output (NixSetPat _ ellipses mAs params) = case mAs of
    (Just (L _ NixSetPatAs {nspaLocation = NixSetPatAsLeading})) -> outputAs <> outputParams
    (Just (L _ NixSetPatAs {nspaLocation = NixSetPatAsTrailing})) -> outputParams <> outputAs
    Nothing -> outputParams
    where
      outputAs = maybe mempty (output . unLoc) mAs
      outputParams = encloseSep "{ " " }" ", " ((output . unLoc <$> params) <> ["..." | ellipses == NixSetPatIsEllipses])
  output (XNixFuncPat _) = extErr
