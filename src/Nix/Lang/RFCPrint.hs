{-# LANGUAGE EmptyCase #-}

-- | RFC-style formatter for fresh syntax trees.
--
-- This formatter is for fresh syntax trees: values you constructed yourself,
-- lowered from another representation, or otherwise do not want to exact-print.
--
-- In other words, this is the “choose a clean layout” renderer, not the
-- “preserve the original file exactly” renderer. For the latter, use
-- 'Nix.Lang.ExactPrint'. If you want this formatter to inherit some layout intent
-- from parsed source, use 'Nix.Lang.RFCPrint.LayoutHints'.
--
-- Example:
--
-- @
-- import Nix.Lang.RFCPrint (formatExpr)
-- import Nix.Lang.Types.Syn (mkApp, mkVar)
--
-- doc = formatExpr (mkApp (mkVar "f") (mkVar "x"))
-- @
module Nix.Lang.RFCPrint
  ( formatExpr,
    formatExprWithHints,
    formatDoc,
    formatDocWithHints,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Lexer.Text (escapeIndentedText, escapedChars)
import Nix.Lang.RFCPrint.LayoutHints
import Nix.Lang.Types
import Nix.Lang.Types.Syn
import Nix.Lang.Utils (showBinOP)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

newtype FormatEnv = FormatEnv
  { feLayoutHints :: LayoutHints
  }

rootPath :: NodePath
rootPath = []

childPath :: NodePath -> Int -> NodePath
childPath = childNodePath

chooseLayout :: FormatEnv -> NodePath -> ContainerLayout -> ContainerLayout
chooseLayout env path fallback =
  maybe fallback resolve (lookupLayoutHint path (feLayoutHints env))
  where
    resolve hint = maybe fallback id (lhContainerLayout hint)

formatExpr :: Expr -> Text
formatExpr = formatExprWithHints emptyLayoutHints

formatExprWithHints :: LayoutHints -> Expr -> Text
formatExprWithHints hints = renderStrict . layoutPretty defaultLayoutOptions . formatDocWithHints hints

formatDoc :: Expr -> Doc ann
formatDoc = formatDocWithHints emptyLayoutHints

formatDocWithHints :: LayoutHints -> Expr -> Doc ann
formatDocWithHints hints = formatTop (FormatEnv hints) rootPath

formatTop :: FormatEnv -> NodePath -> Expr -> Doc ann
formatTop env path = formatNode env path

data Assoc
  = AssocLeft
  | AssocRight
  | AssocNone
  deriving (Eq)

precLowest, precImpl, precOr, precAnd, precEq, precCmp, precUpdate, precNot, precAdd, precMul, precConcat, precHasAttr, precNeg, precApp, precSelect, precAtom :: Int
precLowest = 0
precImpl = 1
precOr = 2
precAnd = 3
precEq = 4
precCmp = 5
precUpdate = 6
precNot = 7
precAdd = 8
precMul = 9
precConcat = 10
precHasAttr = 11
precNeg = 12
precApp = 13
precSelect = 14
precAtom = 15

exprPrec :: Expr -> Int
exprPrec = \case
  NixVar {} -> precAtom
  NixLit {} -> precAtom
  NixPar {} -> precAtom
  NixString {} -> precAtom
  NixPath {} -> precAtom
  NixEnvPath {} -> precAtom
  NixSelect _ _ _ (Just _) -> precLowest
  NixSelect {} -> precSelect
  NixApp {} -> precApp
  NixNegApp {} -> precNeg
  NixHasAttr {} -> precHasAttr
  NixBinApp _ op _ _ -> opPrec op
  NixNotApp {} -> precNot
  NixList {} -> precAtom
  NixSet {} -> precAtom
  NixLet {} -> precLowest
  NixIf {} -> precLowest
  NixWith {} -> precLowest
  NixAssert {} -> precLowest
  NixLam {} -> precLowest

opPrec :: BinaryOp -> Int
opPrec = \case
  OpConcat -> precConcat
  OpMul -> precMul
  OpDiv -> precMul
  OpAdd -> precAdd
  OpSub -> precAdd
  OpUpdate -> precUpdate
  OpLT -> precCmp
  OpLE -> precCmp
  OpGT -> precCmp
  OpGE -> precCmp
  OpEq -> precEq
  OpNEq -> precEq
  OpAnd -> precAnd
  OpOr -> precOr
  OpImpl -> precImpl

opAssoc :: BinaryOp -> Assoc
opAssoc = \case
  OpConcat -> AssocRight
  OpUpdate -> AssocRight
  OpEq -> AssocNone
  OpNEq -> AssocNone
  _ -> AssocLeft

formatChild :: FormatEnv -> NodePath -> Int -> Bool -> Expr -> Doc ann
formatChild env path parentPrec allowSame expr =
  parenthesize (childPrec < parentPrec || (childPrec == parentPrec && not allowSame)) (formatNode env path expr)
  where
    childPrec = exprPrec expr

formatPrefixChild :: FormatEnv -> NodePath -> Int -> Expr -> Doc ann
formatPrefixChild env path parentPrec expr =
  parenthesize (childPrec <= parentPrec) (formatNode env path expr)
  where
    childPrec = exprPrec expr

formatBinaryLeftChild :: FormatEnv -> NodePath -> BinaryOp -> Expr -> Doc ann
formatBinaryLeftChild env path op =
  formatChild env path (opPrec op) (opAssoc op == AssocLeft)

formatBinaryRightChild :: FormatEnv -> NodePath -> BinaryOp -> Expr -> Doc ann
formatBinaryRightChild env path op =
  formatChild env path (opPrec op) (opAssoc op == AssocRight)

formatNode :: FormatEnv -> NodePath -> Expr -> Doc ann
formatNode env path = \case
  NixVar _ name -> pretty name
  NixLit _ lit -> formatLit lit
  NixPar _ expr -> parens (formatTop env (childPath path 0) expr)
  NixString _ str -> formatString env path str
  NixPath _ p -> formatPath env path p
  NixEnvPath _ p -> "<" <> pretty p <> ">"
  NixLam _ pat@(NixVarPat _ _) expr -> group $ hang 2 (formatFuncPat env (childPath path 0) pat <> ":" <+> formatTop env (childPath path 1) expr)
  NixLam _ pat expr ->
    case chooseLayout env path PreferMultiline of
      PreferInline -> group $ hang 2 (formatFuncPat env (childPath path 0) pat <> ":" <+> formatTop env (childPath path 1) expr)
      PreferMultiline -> formatFuncPat env (childPath path 0) pat <> ":" <> hardline <> formatTop env (childPath path 1) expr
  NixApp _ f x -> group $ hang 2 (formatChild env (childPath path 0) precApp True f <+> formatChild env (childPath path 1) precApp False x)
  NixBinApp _ op lhs rhs ->
    group $ hang 2 (formatBinaryLeftChild env (childPath path 0) op lhs <+> pretty (showBinOP op) <+> formatBinaryRightChild env (childPath path 1) op rhs)
  NixNotApp _ expr -> "!" <> formatPrefixChild env (childPath path 0) precNot expr
  NixNegApp _ expr -> "-" <> formatPrefixChild env (childPath path 0) precNeg expr
  NixList _ xs -> formatBracketed env path "[" "]" (zipWith (formatTop env . childPath path) [0 ..] xs)
  NixSet _ recFlag bindings -> formatSet env path recFlag bindings
  NixLet _ bindings expr -> formatLet env path bindings expr
  NixHasAttr _ expr attrPath -> formatPrefixChild env (childPath path 0) precHasAttr expr <+> "?" <+> formatAttrPath env (childPath path 1) attrPath
  NixSelect _ expr attrPath mDefault ->
    formatPrefixChild env (childPath path 0) precSelect expr <> "." <> formatAttrPath env (childPath path 1) attrPath <> maybe mempty (formatSelectDefault env (childPath path 2)) mDefault
  NixIf _ cond t f ->
    group $ "if" <+> formatTop env (childPath path 0) cond <+> "then" <+> formatTop env (childPath path 1) t <+> "else" <+> formatTop env (childPath path 2) f
  NixWith _ scope expr ->
    group $ "with" <+> formatTop env (childPath path 0) scope <> ";" <+> formatTop env (childPath path 1) expr
  NixAssert _ assertion expr ->
    "assert "
      <> formatTop env (childPath path 0) assertion
      <> ";"
      <> hardline
      <> formatTop env (childPath path 1) expr

formatLit :: Lit -> Doc ann
formatLit = \case
  NixUri _ uri -> pretty uri
  NixInteger _ int -> pretty int
  NixFloat _ float -> pretty float
  NixBoolean _ bool -> if bool then "true" else "false"
  NixNull _ -> "null"

formatString :: FormatEnv -> NodePath -> NString -> Doc ann
formatString env path = \case
  NixDoubleQuotesString _ parts -> dquotes $ mconcat (zipWith (formatDoubleQuotedPart env . childPath path) [0 ..] parts)
  NixDoubleSingleQuotesString _ parts -> "''" <> mconcat (zipWith (formatIndentedPart env . childPath path) [0 ..] parts) <> "''"

formatDoubleQuotedPart :: FormatEnv -> NodePath -> StringPart -> Doc ann
formatDoubleQuotedPart env path = \case
  NixStringLiteral _ text -> pretty (escapeDoubleQuoted text)
  NixStringInterpol _ expr -> "${" <> formatTop env path expr <> "}"

formatIndentedPart :: FormatEnv -> NodePath -> StringPart -> Doc ann
formatIndentedPart env path = \case
  NixStringLiteral _ text -> pretty (escapeIndentedText text)
  NixStringInterpol _ expr -> "${" <> formatTop env path expr <> "}"

formatPath :: FormatEnv -> NodePath -> Path -> Doc ann
formatPath env path = \case
  NixLiteralPath _ literalPath -> pretty literalPath
  NixInterpolPath _ parts -> mconcat (zipWith (formatPathPart env . childPath path) [0 ..] parts)

formatPathPart :: FormatEnv -> NodePath -> StringPart -> Doc ann
formatPathPart env path = \case
  NixStringLiteral _ text -> pretty text
  NixStringInterpol _ expr -> "${" <> formatTop env path expr <> "}"

formatAttrKey :: FormatEnv -> NodePath -> AttrKey -> Doc ann
formatAttrKey env path = \case
  NixStaticAttrKey _ name -> pretty name
  NixDynamicStringAttrKey _ parts -> dquotes $ mconcat (zipWith (formatDoubleQuotedPart env . childPath path) [0 ..] parts)
  NixDynamicInterpolAttrKey _ expr -> "${" <> formatTop env path expr <> "}"

formatAttrPath :: FormatEnv -> NodePath -> AttrPath -> Doc ann
formatAttrPath env path (NixAttrPath _ keys) = hcat $ punctuate "." (zipWith (formatAttrKey env . childPath path) [0 ..] keys)

formatBinding :: FormatEnv -> NodePath -> Binding -> Doc ann
formatBinding env path = \case
  NixNormalBinding _ attrPath expr -> formatAttrPath env (childPath path 0) attrPath <+> "=" <+> formatTop env (childPath path 1) expr <> ";"
  NixInheritBinding _ mScope names ->
    let scopeDoc = maybe mempty (\scope -> " (" <> formatInheritScope env (childPath path 0) scope <> ")") mScope
        namesDoc = case names of
          [] -> mempty
          xs -> " " <> fillSep (zipWith (formatAttrKey env . childPath path) [0 ..] xs)
     in group ("inherit" <> scopeDoc <> namesDoc <> ";")

formatFuncPat :: FormatEnv -> NodePath -> FuncPat -> Doc ann
formatFuncPat env path = \case
  NixVarPat _ name -> pretty name
  NixSetPat _ ellipses mAs bindings ->
    case fmap nspaLocation mAs of
      Just NixSetPatAsLeading -> formatSetPatAsLeading <> formatSetPatParams
      Just NixSetPatAsTrailing -> formatSetPatParams <> formatSetPatAsTrailing
      Nothing -> formatSetPatParams
    where
      formatSetPatAsLeading = maybe mempty formatSetPatAs mAs
      formatSetPatAsTrailing = maybe mempty formatSetPatAs mAs
      entries = zipWith (formatSetPatBinding env . childPath path) [0 ..] bindings <> ["..." | ellipses == NixSetPatIsEllipses]
      formatSetPatParams =
        case chooseLayout env path PreferMultiline of
          PreferInline -> if null entries then "{}" else encloseSep "{ " " }" ", " entries
          PreferMultiline -> case entries of
            [] -> "{}"
            _ -> vsep (["{"] <> fmap (indent 2) (punctuate "," entries) <> ["}"])

formatSetPatAs :: SetPatAs -> Doc ann
formatSetPatAs NixSetPatAs {..} = case nspaLocation of
  NixSetPatAsLeading -> pretty nspaVar <> "@"
  NixSetPatAsTrailing -> "@" <> pretty nspaVar

formatSetPatBinding :: FormatEnv -> NodePath -> SetPatBinding -> Doc ann
formatSetPatBinding env path NixSetPatBinding {..} = pretty nspbVar <> maybe mempty ((" ? " <>) . formatTop env (childPath path 0)) nspbDefault

formatSet :: FormatEnv -> NodePath -> NixSetIsRecursive -> [Binding] -> Doc ann
formatSet env path recFlag bindings =
  case bindings of
    [] -> case recFlag of
      NixSetRecursive -> "rec {}"
      NixSetNonRecursive -> "{}"
    _ -> case chooseLayout env path PreferMultiline of
      PreferInline -> single
      PreferMultiline -> multi
  where
    prefix = case recFlag of
      NixSetRecursive -> "rec "
      NixSetNonRecursive -> mempty
    single = prefix <> "{ " <> hsep (zipWith (formatBinding env . childPath path) [0 ..] bindings) <+> "}"
    multi = vsep [prefix <> "{", indent 2 (vsep (zipWith (formatBinding env . childPath path) [0 ..] bindings)), "}"]

formatLet :: FormatEnv -> NodePath -> [Binding] -> Expr -> Doc ann
formatLet env path bindings expr =
  case bindings of
    [] -> group ("let in" <+> formatTop env (childPath path 0) expr)
    _ -> case chooseLayout env path PreferMultiline of
      PreferInline -> single
      PreferMultiline -> multi
  where
    single = "let" <+> hsep (zipWith (formatBinding env . childPath path) [0 ..] bindings) <+> "in" <+> formatTop env (childPath path (length bindings)) expr
    multi = vsep ["let", indent 2 (vsep (zipWith (formatBinding env . childPath path) [0 ..] bindings)), "in", formatTop env (childPath path (length bindings)) expr]

formatInheritScope :: FormatEnv -> NodePath -> Expr -> Doc ann
formatInheritScope env path = formatTop env path

formatBracketed :: FormatEnv -> NodePath -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
formatBracketed env path open close docs =
  case docs of
    [] -> open <> close
    _ -> case chooseLayout env path PreferMultiline of
      PreferInline -> single
      PreferMultiline -> multi
  where
    single = open <+> hsep docs <+> close
    multi = vsep [open, indent 2 (vsep docs), close]

formatSelectDefault :: FormatEnv -> NodePath -> Expr -> Doc ann
formatSelectDefault env path expr = " or " <> parenthesize (needsKeywordParens expr) (formatTop env path expr)

needsKeywordParens :: Expr -> Bool
needsKeywordParens = \case
  NixIf {} -> True
  NixWith {} -> True
  NixAssert {} -> True
  NixLet {} -> True
  NixLam {} -> True
  NixSelect _ _ _ (Just _) -> True
  _ -> False

parenthesize :: Bool -> Doc ann -> Doc ann
parenthesize needs doc
  | needs = parens doc
  | otherwise = doc

escapeDoubleQuoted :: Text -> Text
escapeDoubleQuoted = go
  where
    go text
      | T.null text = mempty
      | "${" `T.isPrefixOf` text = "\\${" <> go (T.drop 2 text)
      | otherwise =
          let (char, rest) = (T.head text, T.tail text)
           in escapeChar char <> go rest

    escapeChar char = case lookupEscape char of
      Just code -> "\\" <> T.singleton code
      Nothing -> T.singleton char

    lookupEscape char = lookup char escapes

    escapes =
      [ (decoded, encoded)
      | (encoded, decoded) <- escapedChars,
        decoded `notElem` ['{', '.']
      ]
