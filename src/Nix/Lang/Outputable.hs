-- | Basic pretty-printing for Nix AST values.
--
-- This is the simple renderer shared across the AST. It does not try to preserve
-- parsed layout, and it is less opinionated than 'Nix.Lang.RFCPrint'.
module Nix.Lang.Outputable where

import Data.Data (Proxy (..))
import Data.Text (Text)
import Nix.Lang.Lexer.Text
import Nix.Lang.Types
import Nix.Lang.Types.Ps
import Nix.Lang.Types.Syn (Syn)
import Nix.Lang.Utils
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

class Outputable a where
  output :: a -> Doc ann

class OutputableNames p where
  outputVarName :: Proxy p -> NixVarName p -> Doc ann
  outputBinderName :: Proxy p -> NixBinderName p -> Doc ann
  outputAttrName :: Proxy p -> NixAttrName p -> Doc ann

instance OutputableNames Ps where
  outputVarName _ = pretty
  outputBinderName _ = pretty
  outputAttrName _ = pretty

instance OutputableNames Syn where
  outputVarName _ = pretty
  outputBinderName _ = pretty
  outputAttrName _ = pretty

renderToText :: (Outputable a) => a -> Text
renderToText = renderStrict . layoutPretty defaultLayoutOptions . output

extErr :: a
extErr = error "unimplemented for extension"

unwrapText :: UnXRec p => Proxy p -> XRec p Text -> Text
unwrapText = unXRec

unwrapVarName :: UnXRec p => Proxy p -> LNixVarName p -> NixVarName p
unwrapVarName = unXRec

unwrapBinderName :: UnXRec p => Proxy p -> LNixBinderName p -> NixBinderName p
unwrapBinderName = unXRec

unwrapAttrName :: UnXRec p => Proxy p -> LNixAttrName p -> NixAttrName p
unwrapAttrName = unXRec

unwrapLit :: UnXRec p => Proxy p -> LNixLit p -> NixLit p
unwrapLit = unXRec

unwrapExpr :: UnXRec p => Proxy p -> LNixExpr p -> NixExpr p
unwrapExpr = unXRec

unwrapString :: UnXRec p => Proxy p -> LNixString p -> NixString p
unwrapString = unXRec

unwrapPath :: UnXRec p => Proxy p -> LNixPath p -> NixPath p
unwrapPath = unXRec

unwrapAttrKey :: UnXRec p => Proxy p -> LNixAttrKey p -> NixAttrKey p
unwrapAttrKey = unXRec

unwrapAttrPath :: UnXRec p => Proxy p -> LNixAttrPath p -> NixAttrPath p
unwrapAttrPath = unXRec

unwrapBinding :: UnXRec p => Proxy p -> LNixBinding p -> NixBinding p
unwrapBinding = unXRec

unwrapFuncPat :: UnXRec p => Proxy p -> LNixFuncPat p -> NixFuncPat p
unwrapFuncPat = unXRec

unwrapBindings :: UnXRec p => Proxy p -> LNixBindings p -> [LNixBinding p]
unwrapBindings = unXRec

unwrapStringPart :: UnXRec p => Proxy p -> LNixStringPart p -> NixStringPart p
unwrapStringPart = unXRec

unwrapSetPatAs :: UnXRec p => Proxy p -> LNixSetPatAs p -> NixSetPatAs p
unwrapSetPatAs = unXRec

unwrapSetPatBinding :: UnXRec p => Proxy p -> LNixSetPatBinding p -> NixSetPatBinding p
unwrapSetPatBinding = unXRec

outputLitX :: UnXRec p => Proxy p -> LNixLit p -> Doc ann
outputLitX proxy lit = output (unwrapLit proxy lit)

outputExprX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixExpr p -> Doc ann
outputExprX proxy expr = output (unwrapExpr proxy expr)

outputStringX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixString p -> Doc ann
outputStringX proxy str = output (unwrapString proxy str)

outputPathX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixPath p -> Doc ann
outputPathX proxy path = output (unwrapPath proxy path)

outputAttrKeyX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixAttrKey p -> Doc ann
outputAttrKeyX proxy key = output (unwrapAttrKey proxy key)

outputAttrPathX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixAttrPath p -> Doc ann
outputAttrPathX proxy path = output (unwrapAttrPath proxy path)

outputBindingX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixBinding p -> Doc ann
outputBindingX proxy binding = output (unwrapBinding proxy binding)

outputFuncPatX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixFuncPat p -> Doc ann
outputFuncPatX proxy pat = output (unwrapFuncPat proxy pat)

outputSetPatBindingX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixSetPatBinding p -> Doc ann
outputSetPatBindingX proxy binding = output (unwrapSetPatBinding proxy binding)

outputSetPatAsX :: (OutputableNames p, UnXRec p) => Proxy p -> LNixSetPatAs p -> Doc ann
outputSetPatAsX proxy asPat = output (unwrapSetPatAs proxy asPat)

instance Outputable (NixLit p) where
  output (NixUri _ uri) = pretty uri
  output (NixInteger _ int) = pretty int
  output (NixFloat _ float) = pretty float
  output (NixBoolean _ bool) = if bool then "true" else "false"
  output (NixNull _) = "null"
  output (XNixLit _) = extErr

instance (OutputableNames p, UnXRec p) => Outputable (NixExpr p) where
  output = go
    where
      proxy = Proxy @p

      go (NixLit _ lit) = outputLitX proxy lit
      go (NixString _ str) = outputStringX proxy str
      go (NixVar _ x) = outputVarName proxy (unwrapVarName proxy x)
      go (NixPar _ x) = parens $ outputExprX proxy x
      go (NixNegApp _ x) = "-" <> outputExprX proxy x
      go (NixNotApp _ x) = "!" <> outputExprX proxy x
      go (NixApp _ f x) = hsep [outputExprX proxy f, outputExprX proxy x]
      go (NixBinApp _ op x y) =
        hsep
          [ outputExprX proxy x,
            pretty $ showBinOP op,
            outputExprX proxy y
          ]
      go (NixHasAttr _ x p) = hsep [outputExprX proxy x, "?", outputAttrPathX proxy p]
      go (NixPath _ p) = outputPathX proxy p
      go (NixEnvPath _ p) = "<" <> pretty (unwrapText proxy p) <> ">"
      go (NixLam _ pat x) = nestedSep [outputFuncPatX proxy pat <> ":", outputExprX proxy x]
      go (NixList _ xs) = case xs of
        [] -> "[]"
        _ -> nestedSep $ "[" : (outputExprX proxy <$> xs) <> ["]"]
      go (NixSet _ NixSetRecursive bindings) = case unwrapBindings proxy bindings of
        [] -> "rec {}"
        xs -> nestedSep $ "rec {" : (outputBindingX proxy <$> xs) <> ["}"]
      go (NixSet _ NixSetNonRecursive bindings) = case unwrapBindings proxy bindings of
        [] -> "{}"
        xs -> nestedSep $ "{" : (outputBindingX proxy <$> xs) <> ["}"]
      go (NixSelect _ x p mx) = outputExprX proxy x <> "." <> outputAttrPathX proxy p <> alt
        where
          alt = maybe mempty ((" or " <>) . outputExprX proxy) mx
      go (NixLet _ bindings x) =
        group $
          vsep ["let", indent 2 (vsep $ outputBindingX proxy <$> unwrapBindings proxy bindings), "in " <> outputExprX proxy x]
      go (NixIf _ cond t f) = nestedSep ["if " <> outputExprX proxy cond, "then " <> outputExprX proxy t, "else " <> outputExprX proxy f]
      go (NixWith _ s x) = nestedSep ["with " <> outputExprX proxy s <> ";", outputExprX proxy x]
      go (NixAssert _ s x) = nestedSep ["assert " <> outputExprX proxy s <> ";", outputExprX proxy x]
      go (XNixExpr _) = extErr

nestedSep :: [Doc ann] -> Doc ann
nestedSep = group . nest 2 . vsep

outputStringPart :: forall p ann. (OutputableNames p, UnXRec p) => Proxy p -> (Text -> Text) -> NixStringPart p -> Doc ann
outputStringPart _ escape (NixStringLiteral _ text) = pretty $ escape text
outputStringPart proxy _ (NixStringInterpol _ expr) = "${" <> outputExprX proxy expr <> "}"
outputStringPart _ _ (XNixStringPart _) = extErr

instance (OutputableNames p, UnXRec p) => Outputable (NixString p) where
  output = go
    where
      proxy = Proxy @p

      go (NixDoubleQuotesString _ parts) = dquotes $ outputParts $ fmap (unwrapStringPart proxy) parts
        where
          outputParts = mconcat . fmap (outputStringPart proxy escapeDoubleQuotedText)
      go (NixDoubleSingleQuotesString _ parts) = vcat ["''", nest 2 $ outputParts $ fmap (unwrapStringPart proxy) parts, "''"]
        where
          outputParts = mconcat . fmap (outputStringPart proxy escapeIndentedText)
      go (XNixString _) = extErr

instance (OutputableNames p, UnXRec p) => Outputable (NixAttrKey p) where
  output = go
    where
      proxy = Proxy @p

      go (NixStaticAttrKey _ x) = outputAttrName proxy (unwrapAttrName proxy x)
      go (NixDynamicStringAttrKey _ parts) = dquotes $ mconcat $ outputStringPart proxy escapeDoubleQuotedText . unwrapStringPart proxy <$> parts
      go (NixDynamicInterpolAttrKey _ expr) = "${" <> outputExprX proxy expr <> "}"
      go (XNixNixAttrKey _) = extErr

instance (OutputableNames p, UnXRec p) => Outputable (NixAttrPath p) where
  output (NixAttrPath _ keys) = hcat . punctuate dot $ outputAttrKeyX (Proxy @p) <$> keys

instance (OutputableNames p, UnXRec p) => Outputable (NixPath p) where
  output = go
    where
      proxy = Proxy @p

      go (NixLiteralPath _ p) = pretty p
      go (NixInterpolPath _ parts) = hcat $ outputStringPart proxy id . unwrapStringPart proxy <$> parts
      go (XNixPath _) = extErr

instance (OutputableNames p, UnXRec p) => Outputable (NixBinding p) where
  output = go
    where
      proxy = Proxy @p

      go (NixNormalBinding _ p x) = hsep [outputAttrPathX proxy p, "=", outputExprX proxy x] <> ";"
      go (NixInheritBinding _ mScope names) = "inherit" <> outputScope <> outputNames names <> ";"
        where
          outputScope = maybe mempty ((" " <>) . outputExprX proxy) mScope
          outputNames [] = mempty
          outputNames ns = " " <> (align . fillSep $ outputAttrKeyX proxy <$> ns)
      go (XNixBinding _) = extErr

instance (OutputableNames p, UnXRec p) => Outputable (NixSetPatBinding p) where
  output NixSetPatBinding {..} = outputBinderName proxy (unwrapBinderName proxy nspbVar) <> outputDefault
    where
      proxy = Proxy @p
      outputDefault = maybe mempty ((" ? " <>) . outputExprX proxy) nspbDefault

instance (OutputableNames p, UnXRec p) => Outputable (NixSetPatAs p) where
  output NixSetPatAs {..} = case nspaLocation of
    NixSetPatAsLeading -> outputNm <> "@"
    NixSetPatAsTrailing -> "@" <> outputNm
    where
      proxy = Proxy @p
      outputNm = outputBinderName proxy $ unwrapBinderName proxy nspaVar

instance (OutputableNames p, UnXRec p) => Outputable (NixFuncPat p) where
  output = go
    where
      proxy = Proxy @p

      go (NixVarPat _ x) = outputBinderName proxy (unwrapBinderName proxy x)
      go (NixSetPat _ ellipses mAs params) = case fmap (nspaLocation . unwrapSetPatAs proxy) mAs of
        Just NixSetPatAsLeading -> outputAs <> outputParams
        Just NixSetPatAsTrailing -> outputParams <> outputAs
        Nothing -> outputParams
        where
          outputAs = maybe mempty (outputSetPatAsX proxy) mAs
          outputParams = encloseSep "{ " " }" ", " ((outputSetPatBindingX proxy <$> params) <> ["..." | ellipses == NixSetPatIsEllipses])
      go (XNixFuncPat _) = extErr
