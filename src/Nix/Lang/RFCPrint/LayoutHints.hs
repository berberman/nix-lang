-- | Layout preferences for 'Nix.Lang.RFCPrint', plus helpers for deriving them
-- from parsed source.

-- At the moment the preserved information is limited to container-shaped inline
-- versus multiline choices such as lists, sets, lets, and set patterns, plus a
-- narrow canonical trivia subset used by RFCPrint for selected parsed-source
-- comments and blank lines.
module Nix.Lang.RFCPrint.LayoutHints
  ( NodePath,
    LayoutHints,
    LayoutHint (..),
    ContainerLayout (..),
    CanonicalTrivia (..),
    emptyLayoutHints,
    singletonLayoutHint,
    lookupLayoutHint,
    childNodePath,
    collectLayoutHints,
    lowerParsedExpr,
    fromParsedExpr,
  )
where

import Data.Bifunctor (first)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint.Operations (exprSpan)
import Nix.Lang.Span (Located (..), SrcSpan, srcSpanEndLine, srcSpanStartLine)
import Nix.Lang.Types
import qualified Nix.Lang.Types.Ps as Parsed
import Nix.Lang.Types.Syn
import Nix.Lang.Utils (getLoc, unLoc)

-- | A structural path from the root expression to a nested child.
--
-- Each integer selects the next child in the formatter traversal.
type NodePath = [Int]

-- | A collection of layout preferences for particular nodes.
newtype LayoutHints = LayoutHints (Map NodePath LayoutHint)
  deriving (Eq, Show)

type HintMap = Map NodePath LayoutHint

type LowerResult a = (a, HintMap)

instance Semigroup LayoutHints where
  LayoutHints left <> LayoutHints right = LayoutHints (left <> right)

instance Monoid LayoutHints where
  mempty = emptyLayoutHints

-- | Layout preferences for a single node.
data LayoutHint = LayoutHint
  { -- | Preferred rendering strategy for container-like nodes.
    lhContainerLayout :: Maybe ContainerLayout,
    -- | Canonical trivia that should be emitted before the node.
    lhPriorTrivia :: [CanonicalTrivia],
    -- | Canonical trivia that should be emitted after the node.
    lhFollowingTrivia :: [CanonicalTrivia]
  }
  deriving (Eq, Show)

-- | Preferred shape for a list, set, let-body, or similar container.
data ContainerLayout
  = PreferInline
  | PreferMultiline
  deriving (Eq, Show)

-- | Canonicalized trivia carried from parsed source into RFCPrint.
data CanonicalTrivia
  = CanonicalLineComment Text
  | CanonicalBlockComment Text
  | CanonicalBlankLine
  deriving (Eq, Show)

-- | No layout preferences.
emptyLayoutHints :: LayoutHints
emptyLayoutHints = LayoutHints Map.empty

-- | A single hint at one 'NodePath'.
singletonLayoutHint :: NodePath -> LayoutHint -> LayoutHints
singletonLayoutHint path hint = LayoutHints (Map.singleton path hint)

-- | Look up the hint for a node path.
lookupLayoutHint :: NodePath -> LayoutHints -> Maybe LayoutHint
lookupLayoutHint key (LayoutHints hints) = Map.lookup key hints

-- | Extend a node path with one child index.
childNodePath :: NodePath -> Int -> NodePath
childNodePath path index = path <> [index]

-- | Extract layout hints from a parsed expression while discarding the lowered
-- expression tree.
collectLayoutHints :: Parsed.Expr -> LayoutHints
collectLayoutHints = snd . fromParsedExpr

-- | Lower a parsed expression to 'Expr' while discarding any extracted layout
-- hints.
lowerParsedExpr :: Parsed.Expr -> Expr
lowerParsedExpr = fst . fromParsedExpr

-- | Lower a parsed expression to formatter input and collect structural layout
-- hints keyed by 'NodePath'.
fromParsedExpr :: Parsed.Expr -> (Expr, LayoutHints)
fromParsedExpr expr = wrapHints (goExpr [] expr)

goExpr :: NodePath -> Parsed.Expr -> LowerResult Expr
goExpr path parsed = addExprHint path parsed $ case parsed of
  NixVar _ name -> pureResult (mkVar (unLoc name))
  NixLit _ lit -> pureResult (mkLit (lowerLit (unLoc lit)))
  NixPar _ inner -> first mkPar (goExpr (childNodePath path 0) (unLoc inner))
  NixString _ str -> first mkString (goString path (unLoc str))
  NixPath _ p -> first mkPath (goPath path (unLoc p))
  NixEnvPath _ p -> pureResult (mkEnvPath (unLoc p))
  NixLam _ pat body ->
    let (pat', patHints) = goFuncPat (childNodePath path 0) (unLoc pat)
        (body', bodyHints) = goExpr (childNodePath path 1) (unLoc body)
     in (mkLam pat' body', patHints <> bodyHints)
  NixApp _ f x ->
    let (f', fHints) = goExpr (childNodePath path 0) (unLoc f)
        (x', xHints) = goExpr (childNodePath path 1) (unLoc x)
     in (mkApp f' x', fHints <> xHints)
  NixBinApp _ op lhs rhs ->
    let (lhs', lhsHints) = goExpr (childNodePath path 0) (unLoc lhs)
        (rhs', rhsHints) = goExpr (childNodePath path 1) (unLoc rhs)
     in (mkBinApp op lhs' rhs', lhsHints <> rhsHints)
  NixNotApp _ inner -> first mkNot (goExpr (childNodePath path 0) (unLoc inner))
  NixNegApp _ inner -> first mkNeg (goExpr (childNodePath path 0) (unLoc inner))
  NixList _ xs -> first mkList (goLocated path goExpr xs)
  NixSet ann recFlag bindings ->
    let (bindings', hints) = goSetBindings path ann (unLoc bindings)
     in (case recFlag of NixSetRecursive -> mkRecSet bindings'; NixSetNonRecursive -> mkSet bindings', hints)
  NixLet _ bindings body ->
    let parsedBindings = unLoc bindings
        (bindings', bindingHints) = goLocated path goBinding parsedBindings
        (body', bodyHints) = goExpr (childNodePath path (length parsedBindings)) (unLoc body)
     in (mkLet bindings' body', bindingHints <> bodyHints)
  NixHasAttr _ inner attrPath ->
    let (inner', innerHints) = goExpr (childNodePath path 0) (unLoc inner)
        (attrPath', attrHints) = goAttrPath Nothing (childNodePath path 1) (unLoc attrPath)
     in (mkHasAttr inner' attrPath', innerHints <> attrHints)
  NixSelect _ inner attrPath mDefault ->
    let (inner', innerHints) = goExpr (childNodePath path 0) (unLoc inner)
        (attrPath', attrHints) = goAttrPath Nothing (childNodePath path 1) (unLoc attrPath)
        (mDefault', defaultHints) = maybe (Nothing, mempty) (first Just . goExpr (childNodePath path 2) . unLoc) mDefault
     in (maybe (mkSelect inner' attrPath') (mkSelectOr inner' attrPath') mDefault', innerHints <> attrHints <> defaultHints)
  NixIf _ c t f ->
    let (c', cHints) = goExpr (childNodePath path 0) (unLoc c)
        (t', tHints) = goExpr (childNodePath path 1) (unLoc t)
        (f', fHints) = goExpr (childNodePath path 2) (unLoc f)
     in (mkIf c' t' f', cHints <> tHints <> fHints)
  NixWith _ scope body ->
    let (scope', scopeHints) = goExpr (childNodePath path 0) (unLoc scope)
        (body', bodyHints) = goExpr (childNodePath path 1) (unLoc body)
     in (mkWith scope' body', scopeHints <> bodyHints)
  NixAssert _ assertion body ->
    let (assertion', assertionHints) = goExpr (childNodePath path 0) (unLoc assertion)
        (body', bodyHints) = goExpr (childNodePath path 1) (unLoc body)
     in (mkAssert assertion' body', assertionHints <> bodyHints)

goString :: NodePath -> Parsed.NString -> LowerResult NString
goString path = \case
  NixDoubleQuotesString _ parts -> first mkDoubleQuotedString (goLocated path goStringPart parts)
  NixDoubleSingleQuotesString _ parts -> first mkIndentedNString (goLocated path goStringPart parts)

goPath :: NodePath -> Parsed.Path -> LowerResult Path
goPath path = \case
  NixLiteralPath _ p -> pureResult (mkLiteralPath p)
  NixInterpolPath _ parts -> first mkInterpolatedPath (goLocated path goStringPart parts)

goStringPart :: NodePath -> NixStringPart Parsed.Ps -> LowerResult StringPart
goStringPart path = \case
  NixStringLiteral _ text -> pureResult (mkStringLiteral text)
  NixStringInterpol _ inner -> first mkInterpol (goExpr path (unLoc inner))

goAttrKey :: NodePath -> Parsed.AttrKey -> LowerResult AttrKey
goAttrKey path = \case
  NixStaticAttrKey _ name -> pureResult (mkAttr (unLoc name))
  NixDynamicStringAttrKey _ parts ->
    let (parts', hints) = goLocated path goStringPart parts
     in (case parts' of [NixStringLiteral _ text] -> mkQuotedAttr text; _ -> mkDynamicAttr parts', hints)
  NixDynamicInterpolAttrKey _ inner -> first mkInterpolatedAttr (goExpr path (unLoc inner))

goAttrPath :: Maybe SrcSpan -> NodePath -> Parsed.AttrPath -> LowerResult AttrPath
goAttrPath mBeforeSpan path attrPath = addAttrPathHint mBeforeSpan path attrPath $ case attrPath of
  NixAttrPath _ keys -> first mkAttrPath (goLocated path goAttrKey keys)

goBinding :: NodePath -> Parsed.Binding -> LowerResult Binding
goBinding = goBindingWithBefore Nothing

goBindingWithBefore :: Maybe SrcSpan -> NodePath -> Parsed.Binding -> LowerResult Binding
goBindingWithBefore mBeforeSpan path binding = addBindingHint path binding $ case binding of
  NixNormalBinding _ attrPath value ->
    let (attrPath', attrHints) = goAttrPath mBeforeSpan (childNodePath path 0) (unLoc attrPath)
        (value', valueHints) = goExpr (childNodePath path 1) (unLoc value)
     in (mkNormalBinding attrPath' value', attrHints <> valueHints)
  NixInheritBinding _ mScope keys ->
    let (mScope', scopeHints) = maybe (Nothing, mempty) (first Just . goExpr (childNodePath path 0) . stripParens . unLoc) mScope
        (keys', keyHints) = goLocated path goAttrKey keys
     in (maybe mkInheritKeys mkInheritFromKeys mScope' keys', scopeHints <> keyHints)

goSetBindings :: NodePath -> AnnSet -> [Located Parsed.Binding] -> LowerResult [Binding]
goSetBindings path ann bindings = (reverse reversedBindings, accHints)
  where
    openSpan = annTokenSrcSpan (asOpenC ann)
    (reversedBindings, _, accHints) =
      foldl' step ([], openSpan, mempty) (zip [0 :: Int ..] bindings)

    step (!bindingsSoFar, mPreviousAnchorSpan, !hints) (i, locatedBinding) =
      let (binding', bindingHints) = goBindingWithBefore mPreviousAnchorSpan (childNodePath path i) (unLoc locatedBinding)
        in (binding' : bindingsSoFar, Just (getLoc locatedBinding), hints <> bindingHints)

goFuncPat :: NodePath -> Parsed.FuncPat -> LowerResult FuncPat
goFuncPat path pat = addPatHint path pat $ case pat of
  NixVarPat _ name -> pureResult (mkVarPat (unLoc name))
  NixSetPat _ ellipses mAs bindings ->
    let mAs' = lowerSetPatAs . unLoc <$> mAs
        (bindings', bindingHints) = goLocated path goSetPatBinding bindings
     in (mkSetPat ellipses mAs' bindings', bindingHints)

goSetPatBinding :: NodePath -> Parsed.SetPatBinding -> LowerResult SetPatBinding
goSetPatBinding path NixSetPatBinding {..} =
  let (mDefault', hints) = maybe (Nothing, mempty) (first Just . goExpr (childNodePath path 0) . unLoc) nspbDefault
   in (mkSetPatBinding (unLoc nspbVar) mDefault', hints)

goLocated :: NodePath -> (NodePath -> a -> LowerResult b) -> [Located a] -> LowerResult [b]
goLocated path lowerAtPath = foldr step ([], mempty) . zip [0 :: Int ..]
  where
    step (i, located) (loweredItems, hints) =
      let (loweredItem, itemHints) = lowerAtPath (childNodePath path i) (unLoc located)
       in (loweredItem : loweredItems, itemHints <> hints)

addExprHint :: NodePath -> Parsed.Expr -> LowerResult Expr -> LowerResult Expr
addExprHint path parsedExpr = addHint path (exprLayout parsedExpr) (nodeCommentHint parsedExpr)

addPatHint :: NodePath -> Parsed.FuncPat -> LowerResult FuncPat -> LowerResult FuncPat
addPatHint path pat = addHint path (patLayout pat) (funcPatCommentHint pat)

addBindingHint :: NodePath -> Parsed.Binding -> LowerResult Binding -> LowerResult Binding
addBindingHint path binding = addHint path Nothing (bindingCommentHint binding)

addAttrPathHint :: Maybe SrcSpan -> NodePath -> Parsed.AttrPath -> LowerResult AttrPath -> LowerResult AttrPath
addAttrPathHint mBefore path attrPath = addHint path Nothing (attrPathCommentHint mBefore attrPath)

addHint :: NodePath -> Maybe ContainerLayout -> ([CanonicalTrivia], [CanonicalTrivia]) -> LowerResult a -> LowerResult a
addHint path mLayout (prior, following) (x, hints) =
  let maybeHint = case (mLayout, prior, following) of
        (Nothing, [], []) -> Nothing
        _ -> Just (LayoutHint mLayout prior following)
   in (x, maybe hints (\hint -> Map.insert path hint hints) maybeHint)

wrapHints :: LowerResult Expr -> (Expr, LayoutHints)
wrapHints (expr, hints) = (expr, LayoutHints hints)

commentTrivia :: [Located Comment] -> [CanonicalTrivia]
commentTrivia = fmap render
  where
    render (L _ comment) = case comment of
      LineComment txt -> CanonicalLineComment txt
      BlockComment txt -> CanonicalBlockComment txt

nodeCommentHint :: Parsed.Expr -> ([CanonicalTrivia], [CanonicalTrivia])
nodeCommentHint expr =
  case extractExprComments expr of
    NodeComments prior following ->
      (spacedTrivia Nothing prior (Just (exprSpan expr)), spacedTrivia (Just (exprSpan expr)) following Nothing)

funcPatCommentHint :: Parsed.FuncPat -> ([CanonicalTrivia], [CanonicalTrivia])
funcPatCommentHint pat =
  case extractFuncPatComments pat of
    NodeComments prior following ->
      let anchor = funcPatSpan pat
       in (spacedTrivia Nothing prior anchor, spacedTrivia anchor following Nothing)

noTrivia :: ([CanonicalTrivia], [CanonicalTrivia])
noTrivia = ([], [])

bindingCommentHint :: Parsed.Binding -> ([CanonicalTrivia], [CanonicalTrivia])
bindingCommentHint = \case
  NixNormalBinding {} -> noTrivia
  NixInheritBinding ann _ _ ->
    let NodeComments prior following = annComments ann
        anchor = annSrcSpan ann
     in (spacedTrivia Nothing prior anchor, spacedTrivia anchor following Nothing)

attrPathCommentHint :: Maybe SrcSpan -> Parsed.AttrPath -> ([CanonicalTrivia], [CanonicalTrivia])
attrPathCommentHint mBefore attrPath =
  case extractAttrPathComments attrPath of
    NodeComments prior following ->
      let anchor = attrPathSpan attrPath
       in (spacedTrivia mBefore prior anchor, spacedTrivia anchor following Nothing)

spacedTrivia :: Maybe SrcSpan -> [Located Comment] -> Maybe SrcSpan -> [CanonicalTrivia]
spacedTrivia mBefore comments mAfter =
  prependGap mBefore uniqueComments
    <> interleaveComments uniqueComments
    <> appendGap uniqueComments mAfter
  where
    uniqueComments = nub comments

    interleaveComments [] = []
    interleaveComments [comment] = commentTrivia [comment]
    interleaveComments (left@(L leftSpan _) : rest@(L rightSpan _ : _)) =
      commentTrivia [left]
        <> [CanonicalBlankLine | srcSpanStartLine rightSpan - srcSpanEndLine leftSpan >= 2]
        <> interleaveComments rest

    prependGap (Just before) (L firstSpan _ : _) | srcSpanStartLine firstSpan - srcSpanEndLine before >= 2 = [CanonicalBlankLine]
    prependGap _ _ = []

    appendGap [L lastSpan _] (Just after)
      | srcSpanStartLine after - srcSpanEndLine lastSpan >= 2 = [CanonicalBlankLine]
    appendGap [] (Just _) = []
    appendGap _ _ = []

funcPatSpan :: Parsed.FuncPat -> Maybe SrcSpan
funcPatSpan = \case
  NixVarPat ann _ -> annSrcSpan ann
  NixSetPat ann _ _ _ -> annSrcSpan ann

attrPathSpan :: Parsed.AttrPath -> Maybe SrcSpan
attrPathSpan = \case
  NixAttrPath ann _ -> annSrcSpan ann

extractExprComments :: Parsed.Expr -> NodeComments
extractExprComments = \case
  NixVar ann _ -> annComments ann
  NixLit ann _ -> annComments ann
  NixPar ann _ -> annComments ann
  NixString ann _ -> annComments ann
  NixPath ann _ -> annComments ann
  NixEnvPath ann _ -> annComments ann
  NixLam ann _ _ -> annComments ann
  NixApp ann _ _ -> annComments ann
  NixBinApp ann _ _ _ -> annComments ann
  NixNotApp ann _ -> annComments ann
  NixNegApp ann _ -> annComments ann
  NixList ann _ -> annComments ann
  NixSet ann _ _ -> annComments ann
  NixLet ann _ _ -> annComments ann
  NixHasAttr ann _ _ -> annComments ann
  NixSelect ann _ _ _ -> annComments ann
  NixIf ann _ _ _ -> annComments ann
  NixWith ann _ _ -> annComments ann
  NixAssert ann _ _ -> annComments ann

extractFuncPatComments :: Parsed.FuncPat -> NodeComments
extractFuncPatComments = \case
  NixVarPat ann _ -> annComments ann
  NixSetPat ann _ _ _ -> annComments ann

extractAttrPathComments :: Parsed.AttrPath -> NodeComments
extractAttrPathComments = \case
  NixAttrPath ann _ -> annComments ann

exprLayout :: Parsed.Expr -> Maybe ContainerLayout
exprLayout = \case
  NixList ann _ -> Just (tokenPairLayout (alnOpenS ann) (alnCloseS ann))
  NixSet ann _ bindings -> Just (if null (unLoc bindings) then PreferInline else tokenPairLayout (asOpenC ann) (asCloseC ann))
  NixLet ann bindings body -> Just (letLayout ann (unLoc bindings) (unLoc body))
  NixLam _ pat _ -> patLayout (unLoc pat)
  _ -> Nothing

patLayout :: Parsed.FuncPat -> Maybe ContainerLayout
patLayout = \case
  NixSetPat ann _ _ bindings -> Just (if null bindings then PreferInline else tokenPairLayout (aspOpenC ann) (aspCloseC ann))
  _ -> Nothing

tokenPairLayout :: AnnToken -> AnnToken -> ContainerLayout
tokenPairLayout left right =
  case (annTokenSrcSpan left, annTokenSrcSpan right) of
    (Just l, Just r) | srcSpanStartLine l == srcSpanStartLine r -> PreferInline
    _ -> PreferMultiline

letLayout :: AnnLetNode -> [Parsed.LBinding] -> Parsed.Expr -> ContainerLayout
letLayout ann bindings body =
  case (bindings, annTokenSrcSpan (alIn ann)) of
    ([], _) -> PreferInline
    (_, Just inSpan) | srcSpanStartLine inSpan == srcSpanStartLine (exprSpan body) -> PreferInline
    _ -> PreferMultiline

stripParens :: Parsed.Expr -> Parsed.Expr
stripParens (NixPar _ expr') = unLoc expr'
stripParens expr' = expr'

pureResult :: a -> LowerResult a
pureResult x = (x, mempty)

lowerSetPatAs :: Parsed.SetPatAs -> SetPatAs
lowerSetPatAs NixSetPatAs {..} = mkSetPatAs nspaLocation (unLoc nspaVar)

lowerLit :: Parsed.Lit -> Lit
lowerLit = \case
  NixUri _ uri -> mkUriLit uri
  NixInteger _ int -> mkIntegerLit int
  NixFloat _ float -> mkFloatLit float
  NixBoolean _ bool -> mkBooleanLit bool
  NixNull _ -> mkNullLit
