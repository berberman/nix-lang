{-# LANGUAGE EmptyCase #-}

-- | RFC 166-oriented formatter for fresh syntax trees.
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
chooseLayout env path defaultLayout =
  maybe defaultLayout resolve (lookupLayoutHint path (feLayoutHints env))
  where
    resolve layoutHint = maybe defaultLayout id (lhContainerLayout layoutHint)

lookupTrivia :: FormatEnv -> NodePath -> ([CanonicalTrivia], [CanonicalTrivia])
lookupTrivia env path =
  case lookupLayoutHint path (feLayoutHints env) of
    Just hint -> (lhPriorTrivia hint, lhFollowingTrivia hint)
    Nothing -> ([], [])

formatInterpolation :: FormatEnv -> NodePath -> Expr -> Doc ann
formatInterpolation env path expr = "${" <> formatTop env path expr <> "}"

forceMultilineEnv :: FormatEnv -> NodePath -> FormatEnv
forceMultilineEnv env path = env {feLayoutHints = forceMultilineHint path (feLayoutHints env)}

formatExpr :: Expr -> Text
formatExpr = formatExprWithHints emptyLayoutHints

formatExprWithHints :: LayoutHints -> Expr -> Text
formatExprWithHints hints = renderStrict . layoutPretty defaultLayoutOptions . formatDocWithHints hints

formatDoc :: Expr -> Doc ann
formatDoc = formatDocWithHints emptyLayoutHints

formatDocWithHints :: LayoutHints -> Expr -> Doc ann
formatDocWithHints hints expr =
  let env = FormatEnv hints
   in formatTop env rootPath expr

formatTop :: FormatEnv -> NodePath -> Expr -> Doc ann
formatTop env path expr =
  let (priorTrivia, followingTrivia) = lookupTrivia env path
   in applyTrivia priorTrivia (formatNode env path expr) followingTrivia

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
  OpImpl -> AssocRight
  OpEq -> AssocNone
  OpNEq -> AssocNone
  _ -> AssocLeft

formatChild :: FormatEnv -> NodePath -> Int -> Bool -> Expr -> Doc ann
formatChild env path parentPrec allowSamePrecedence expr =
  parenthesize (childPrecedence < parentPrec || (childPrecedence == parentPrec && not allowSamePrecedence)) (formatNode env path expr)
  where
    childPrecedence = exprPrec expr

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
    case chooseLayout env path (defaultLambdaLayout pat) of
      PreferInline -> group $ hang 2 (formatFuncPat env (childPath path 0) pat <> ":" <+> formatTop env (childPath path 1) expr)
      PreferMultiline -> formatFuncPat env (childPath path 0) pat <> ":" <> hardline <> formatTop env (childPath path 1) expr
  app@NixApp {} -> formatApp env path app
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
  NixIf _ cond t f -> formatIf env path cond t f
  NixWith _ scope expr ->
    case wantsForcedMultiline expr of
      True ->
        vsep
          [ "with" <+> formatTop env (childPath path 0) scope <> ";",
            formatForcedMultiline env (childPath path 1) expr
          ]
      False -> group $ "with" <+> formatTop env (childPath path 0) scope <> ";" <+> formatTop env (childPath path 1) expr
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
  NixStringInterpol _ expr -> formatInterpolation env path expr

formatIndentedPart :: FormatEnv -> NodePath -> StringPart -> Doc ann
formatIndentedPart env path = \case
  NixStringLiteral _ text -> pretty (escapeIndentedText text)
  NixStringInterpol _ expr -> formatInterpolation env path expr

formatPath :: FormatEnv -> NodePath -> Path -> Doc ann
formatPath env path = \case
  NixLiteralPath _ literalPath -> pretty literalPath
  NixInterpolPath _ parts -> mconcat (zipWith (formatPathPart env . childPath path) [0 ..] parts)

formatPathPart :: FormatEnv -> NodePath -> StringPart -> Doc ann
formatPathPart env path = \case
  NixStringLiteral _ text -> pretty text
  NixStringInterpol _ expr -> formatInterpolation env path expr

formatAttrKey :: FormatEnv -> NodePath -> AttrKey -> Doc ann
formatAttrKey env path = \case
  NixStaticAttrKey _ name -> pretty name
  NixDynamicStringAttrKey _ parts -> dquotes $ mconcat (zipWith (formatDoubleQuotedPart env . childPath path) [0 ..] parts)
  NixDynamicInterpolAttrKey _ expr -> formatInterpolation env path expr

formatAttrPath :: FormatEnv -> NodePath -> AttrPath -> Doc ann
formatAttrPath env path (NixAttrPath _ keys) =
  let (prior, following) = lookupTrivia env path
      (_, prior') = splitLeadingBlankLine prior
      body = hcat $ punctuate "." (zipWith (formatAttrKey env . childPath path) [0 ..] keys)
   in applyTrivia prior' body following

formatBinding :: FormatEnv -> NodePath -> Binding -> Doc ann
formatBinding env path binding =
  case binding of
    NixNormalBinding _ attrPath expr ->
      case wantsBindingValueMultiline expr of
        True ->
          formatAttrPath env (childPath path 0) attrPath
            <+> "="
            <> hardline
            <> indent 2 (formatForcedMultiline env (childPath path 1) expr)
            <> ";"
        False -> formatAttrPath env (childPath path 0) attrPath <+> "=" <+> formatTop env (childPath path 1) expr <> ";"
    NixInheritBinding _ mScope names ->
      let (prior, following) = lookupTrivia env path
          scopeDoc = maybe mempty (\scope -> " (" <> formatInheritScope env (childPath path 0) scope <> ")") mScope
          keyDocs = zipWith (formatAttrKey env . childPath path) [0 ..] names
          namesDoc = case names of
            [] -> mempty
            _ -> " " <> fillSep keyDocs
          multiline =
            vsep
              [ "inherit" <> scopeDoc,
                indent 2 (vsep keyDocs),
                indent 2 ";"
              ]
          body = if length names >= 4 then multiline else group ("inherit" <> scopeDoc <> namesDoc <> ";")
       in applyTrivia prior body following

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
        case chooseLayout env path (defaultSetPatLayout ellipses bindings) of
          PreferInline ->
            case (ellipses, bindings) of
              (NixSetPatNotEllipses, []) -> "{ }"
              (NixSetPatIsEllipses, []) -> "{ ... }"
              _ -> encloseSep "{ " " }" ", " entries
          PreferMultiline -> case entries of
            [] -> "{ }"
            _ ->
              let bodyEntries = fmap (<> ",") (zipWith const entries bindings)
                  ellipsisEntry = ["..." | ellipses == NixSetPatIsEllipses]
               in vsep (["{"] <> fmap indentEntry (bodyEntries <> ellipsisEntry) <> ["}"])
      indentEntry = indent 2

formatSetPatAs :: SetPatAs -> Doc ann
formatSetPatAs NixSetPatAs {..} = case nspaLocation of
  NixSetPatAsLeading -> pretty nspaVar <> "@"
  NixSetPatAsTrailing -> "@" <> pretty nspaVar

formatSetPatBinding :: FormatEnv -> NodePath -> SetPatBinding -> Doc ann
formatSetPatBinding env path NixSetPatBinding {..} = pretty nspbVar <> maybe mempty ((" ? " <>) . formatTop env (childPath path 0)) nspbDefault

defaultLambdaLayout :: FuncPat -> ContainerLayout
defaultLambdaLayout = \case
  NixVarPat {} -> PreferInline
  NixSetPat _ ellipses _ bindings -> defaultSetPatLayout ellipses bindings

defaultSetPatLayout :: NixSetPatEllipses -> [SetPatBinding] -> ContainerLayout
defaultSetPatLayout ellipses bindings =
  case (ellipses, bindings) of
    (NixSetPatNotEllipses, []) -> PreferInline
    (NixSetPatIsEllipses, []) -> PreferInline
    _ -> PreferMultiline

formatSet :: FormatEnv -> NodePath -> NixSetIsRecursive -> [Binding] -> Doc ann
formatSet env path recFlag bindings =
  case bindings of
    [] -> case recFlag of
      NixSetRecursive -> "rec { }"
      NixSetNonRecursive -> "{ }"
    _ -> case chooseLayout env path PreferMultiline of
      PreferInline -> single
      PreferMultiline -> multi
  where
    prefix = case recFlag of
      NixSetRecursive -> "rec "
      NixSetNonRecursive -> mempty
    single = prefix <> "{ " <> hsep (zipWith (formatBinding env . childPath path) [0 ..] bindings) <+> "}"
    multi =
      let bindingDocs = zipWith (formatBinding env . childPath path) [0 ..] bindings
          renderedBindings = intercalateBindingDocs bindingDocs
          openGap = if firstBindingHasLeadingBlankLine then [mempty] else []
       in vsep ([prefix <> "{"] <> openGap <> renderedBindings <> ["}"])

    firstBindingHasLeadingBlankLine =
      case bindings of
        firstBinding : _ -> bindingLeadingBlankLine env (childPath path 0) firstBinding
        [] -> False

    intercalateBindingDocs [] = []
    intercalateBindingDocs [doc] = [indent 2 doc]
    intercalateBindingDocs (doc : rest) = indent 2 doc : concatMap addBindingGap (zip [1 :: Int ..] rest)

    addBindingGap (i, doc)
      | bindingLeadingBlankLine env (childPath path i) (bindings !! i) = [mempty, indent 2 doc]
      | otherwise = [indent 2 doc]

formatLet :: FormatEnv -> NodePath -> [Binding] -> Expr -> Doc ann
formatLet env path bindings expr =
  case bindings of
    [] -> vsep ["let", "in", formatTop env (childPath path 0) expr]
    _ -> case chooseLayout env path PreferMultiline of
      PreferInline -> single
      PreferMultiline -> multi
  where
    single = "let" <+> hsep (zipWith (formatBinding env . childPath path) [0 ..] bindings) <+> "in" <+> formatTop env (childPath path (length bindings)) expr
    multi = vsep ["let", indent 2 (vsep (zipWith (formatBinding env . childPath path) [0 ..] bindings)), "in", formatTop env (childPath path (length bindings)) expr]

formatInheritScope :: FormatEnv -> NodePath -> Expr -> Doc ann
formatInheritScope env path = formatTop env path

formatApp :: FormatEnv -> NodePath -> Expr -> Doc ann
formatApp env path expr =
  case collectAppChain expr of
    [] -> mempty
    f : args ->
      let headDoc = formatAppHead env (childPath path 0) f
          argDocs = zipWith (formatAppArg env . childPath path) [1 ..] args
       in if any wantsForcedMultiline args
            then foldl (<+>) headDoc argDocs
            else group $ hang 2 (headDoc <+> hsep argDocs)

collectAppChain :: Expr -> [Expr]
collectAppChain = go []
  where
    go acc = \case
      NixApp _ f x -> go (x : acc) f
      other -> other : acc

formatAppHead :: FormatEnv -> NodePath -> Expr -> Doc ann
formatAppHead env path expr = formatChild env path precApp True expr

formatAppArg :: FormatEnv -> NodePath -> Expr -> Doc ann
formatAppArg env path expr =
  case wantsForcedMultiline expr of
    True -> parenthesizeForcedAppArg expr (formatForcedMultiline env path expr)
    False -> formatChild env path precApp False expr

parenthesizeForcedAppArg :: Expr -> Doc ann -> Doc ann
parenthesizeForcedAppArg expr doc
  | exprPrec expr <= precApp = vsep ["(", indent 2 doc, ")"]
  | otherwise = doc

formatForcedMultiline :: FormatEnv -> NodePath -> Expr -> Doc ann
formatForcedMultiline env path expr =
  case expr of
    NixSet _ recFlag bindings -> formatSet (forceMultilineEnv env path) path recFlag bindings
    NixLet _ bindings body -> formatLet (forceMultilineEnv env path) path bindings body
    _ -> formatTop env path expr

wantsForcedMultiline :: Expr -> Bool
wantsForcedMultiline = \case
  NixSet _ _ bindings -> not (null bindings)
  NixLet _ bindings _ -> not (null bindings)
  _ -> False

wantsBindingValueMultiline :: Expr -> Bool
wantsBindingValueMultiline = \case
  NixLet _ bindings _ -> not (null bindings)
  _ -> False

formatBracketed :: FormatEnv -> NodePath -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
formatBracketed env path open close docs =
  case docs of
    [] -> open <+> close
    _ -> case chooseLayout env path PreferMultiline of
      PreferInline -> single
      PreferMultiline -> multi
  where
    single = open <+> hsep docs <+> close
    multi = vsep [open, indent 2 (vsep docs), close]

formatSelectDefault :: FormatEnv -> NodePath -> Expr -> Doc ann
formatSelectDefault env path expr = " or " <> parenthesize (needsKeywordParens expr) (formatTop env path expr)

formatIf :: FormatEnv -> NodePath -> Expr -> Expr -> Expr -> Doc ann
formatIf env path cond t f =
  case f of
    NixIf _ nestedCond nestedThen nestedElse ->
      vsep
        [ "if" <+> formatTop env (childPath path 0) cond <+> "then",
          indent 2 (formatTop env (childPath path 1) t),
          formatElseIf env (childPath path 2) nestedCond nestedThen nestedElse
        ]
    _ -> group $ "if" <+> formatTop env (childPath path 0) cond <+> "then" <+> formatTop env (childPath path 1) t <+> "else" <+> formatTop env (childPath path 2) f

formatElseIf :: FormatEnv -> NodePath -> Expr -> Expr -> Expr -> Doc ann
formatElseIf env path cond t f =
  case f of
    NixIf _ nestedCond nestedThen nestedElse ->
      vsep
        [ "else if" <+> formatTop env (childPath path 0) cond <+> "then",
          indent 2 (formatTop env (childPath path 1) t),
          formatElseIf env (childPath path 2) nestedCond nestedThen nestedElse
        ]
    _ ->
      vsep
        [ "else if" <+> formatTop env (childPath path 0) cond <+> "then",
          indent 2 (formatTop env (childPath path 1) t),
          "else",
          indent 2 (formatTop env (childPath path 2) f)
        ]

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

formatCanonicalTrivia :: CanonicalTrivia -> Doc ann
formatCanonicalTrivia = \case
  CanonicalLineComment txt -> "#" <> pretty txt
  CanonicalBlockComment txt -> "/*" <> pretty txt <> "*/"
  CanonicalBlankLine -> mempty

applyTrivia :: [CanonicalTrivia] -> Doc ann -> [CanonicalTrivia] -> Doc ann
applyTrivia prior body following = prepend prior (append following body)
  where
    prepend [] doc = doc
    prepend (CanonicalBlankLine : ts) doc = hardline <> prepend ts doc
    prepend (trivia : ts) doc = formatCanonicalTrivia trivia <> hardline <> prepend ts doc

    append [] doc = doc
    append (CanonicalBlankLine : ts) doc = doc <> hardline <> append ts mempty
    append (trivia : ts) doc = doc <> hardline <> formatCanonicalTrivia trivia <> append ts mempty

splitLeadingBlankLine :: [CanonicalTrivia] -> (Bool, [CanonicalTrivia])
splitLeadingBlankLine (CanonicalBlankLine : xs) = (True, xs)
splitLeadingBlankLine xs = (False, xs)

bindingLeadingBlankLine :: FormatEnv -> NodePath -> Binding -> Bool
bindingLeadingBlankLine env path = \case
  NixNormalBinding _ _ _ ->
    case lookupTrivia env (childPath path 0) of
      (prior, _) -> fst (splitLeadingBlankLine prior)
  NixInheritBinding _ _ _ ->
    case lookupTrivia env path of
      (prior, _) -> fst (splitLeadingBlankLine prior)

forceMultilineHint :: NodePath -> LayoutHints -> LayoutHints
forceMultilineHint path hints =
  case lookupLayoutHint path hints of
    Just hint ->
      hints <> singletonLayoutHint path (LayoutHint (Just PreferMultiline) (lhPriorTrivia hint) (lhFollowingTrivia hint))
    Nothing -> hints <> singletonLayoutHint path (LayoutHint (Just PreferMultiline) [] [])

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
