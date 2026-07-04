-- | Layout preferences for 'Nix.Lang.RFCPrint', plus helpers for deriving them
-- from parsed source.
module Nix.Lang.RFCPrint.LayoutHints
  ( NodePath,
    LayoutHints,
    LayoutHint (..),
    ContainerLayout (..),
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
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint.Operations (exprSpan)
import Nix.Lang.Span (Located, srcSpanStartLine)
import Nix.Lang.Types
import qualified Nix.Lang.Types.Ps as Parsed
import Nix.Lang.Types.Syn
import Nix.Lang.Utils (unLoc)

-- | A structural path from the root expression to a nested child.
--
-- Each integer selects the next child in the formatter traversal.
type NodePath = [Int]

-- | A collection of layout preferences for particular nodes.
newtype LayoutHints = LayoutHints (Map NodePath LayoutHint)
  deriving (Eq, Show)

instance Semigroup LayoutHints where
  LayoutHints left <> LayoutHints right = LayoutHints (left <> right)

instance Monoid LayoutHints where
  mempty = emptyLayoutHints

-- | Layout preferences for a single node.
newtype LayoutHint = LayoutHint
  { -- | Preferred rendering strategy for container-like nodes.
    lhContainerLayout :: Maybe ContainerLayout
  }
  deriving (Eq, Show)

-- | Preferred shape for a list, set, let-body, or similar container.
data ContainerLayout
  = PreferInline
  | PreferMultiline
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

goExpr :: NodePath -> Parsed.Expr -> (Expr, Map NodePath LayoutHint)
goExpr path parsedExpr = addExprHint path parsedExpr $ case parsedExpr of
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
  NixSet _ recFlag bindings ->
    let (bindings', hints) = goLocated path goBinding (unLoc bindings)
     in (case recFlag of NixSetRecursive -> mkRecSet bindings'; NixSetNonRecursive -> mkSet bindings', hints)
  NixLet _ bindings body ->
    let bs = unLoc bindings
        (bindings', bindingHints) = goLocated path goBinding bs
        (body', bodyHints) = goExpr (childNodePath path (length bs)) (unLoc body)
     in (mkLet bindings' body', bindingHints <> bodyHints)
  NixHasAttr _ inner attrPath ->
    let (inner', innerHints) = goExpr (childNodePath path 0) (unLoc inner)
        (attrPath', attrHints) = goAttrPath (childNodePath path 1) (unLoc attrPath)
     in (mkHasAttr inner' attrPath', innerHints <> attrHints)
  NixSelect _ inner attrPath mDefault ->
    let (inner', innerHints) = goExpr (childNodePath path 0) (unLoc inner)
        (attrPath', attrHints) = goAttrPath (childNodePath path 1) (unLoc attrPath)
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

goString :: NodePath -> Parsed.NString -> (NString, Map NodePath LayoutHint)
goString path = \case
  NixDoubleQuotesString _ parts -> first mkDoubleQuotedString (goLocated path goStringPart parts)
  NixDoubleSingleQuotesString _ parts -> first mkIndentedNString (goLocated path goStringPart parts)

goPath :: NodePath -> Parsed.Path -> (Path, Map NodePath LayoutHint)
goPath path = \case
  NixLiteralPath _ p -> pureResult (mkLiteralPath p)
  NixInterpolPath _ parts -> first mkInterpolatedPath (goLocated path goStringPart parts)

goStringPart :: NodePath -> NixStringPart Parsed.Ps -> (StringPart, Map NodePath LayoutHint)
goStringPart path = \case
  NixStringLiteral _ text -> pureResult (mkStringLiteral text)
  NixStringInterpol _ inner -> first mkInterpol (goExpr path (unLoc inner))

goAttrKey :: NodePath -> Parsed.AttrKey -> (AttrKey, Map NodePath LayoutHint)
goAttrKey path = \case
  NixStaticAttrKey _ name -> pureResult (mkAttr (unLoc name))
  NixDynamicStringAttrKey _ parts ->
    let (parts', hints) = goLocated path goStringPart parts
     in (case parts' of [NixStringLiteral _ text] -> mkQuotedAttr text; _ -> mkDynamicAttr parts', hints)
  NixDynamicInterpolAttrKey _ inner -> first mkInterpolatedAttr (goExpr path (unLoc inner))

goAttrPath :: NodePath -> Parsed.AttrPath -> (AttrPath, Map NodePath LayoutHint)
goAttrPath path (NixAttrPath _ keys) = first mkAttrPath (goLocated path goAttrKey keys)

goBinding :: NodePath -> Parsed.Binding -> (Binding, Map NodePath LayoutHint)
goBinding path = \case
  NixNormalBinding _ attrPath value ->
    let (attrPath', attrHints) = goAttrPath (childNodePath path 0) (unLoc attrPath)
        (value', valueHints) = goExpr (childNodePath path 1) (unLoc value)
     in (mkNormalBinding attrPath' value', attrHints <> valueHints)
  NixInheritBinding _ mScope keys ->
    let (mScope', scopeHints) = maybe (Nothing, mempty) (first Just . goExpr (childNodePath path 0) . stripParens . unLoc) mScope
        (keys', keyHints) = goLocated path goAttrKey keys
     in (maybe mkInheritKeys mkInheritFromKeys mScope' keys', scopeHints <> keyHints)

goFuncPat :: NodePath -> Parsed.FuncPat -> (FuncPat, Map NodePath LayoutHint)
goFuncPat path pat = addPatHint path pat $ case pat of
  NixVarPat _ name -> pureResult (mkVarPat (unLoc name))
  NixSetPat _ ellipses mAs bindings ->
    let mAs' = lowerSetPatAs . unLoc <$> mAs
        (bindings', bindingHints) = goLocated path goSetPatBinding bindings
     in (mkSetPat ellipses mAs' bindings', bindingHints)

goSetPatBinding :: NodePath -> Parsed.SetPatBinding -> (SetPatBinding, Map NodePath LayoutHint)
goSetPatBinding path NixSetPatBinding {..} =
  let (mDefault', hints) = maybe (Nothing, mempty) (first Just . goExpr (childNodePath path 0) . unLoc) nspbDefault
   in (mkSetPatBinding (unLoc nspbVar) mDefault', hints)

goLocated :: NodePath -> (NodePath -> a -> (b, Map NodePath LayoutHint)) -> [Located a] -> ([b], Map NodePath LayoutHint)
goLocated path f = foldr step ([], mempty) . zip [0 :: Int ..]
  where
    step (i, located) (xs, hints) =
      let (x, xHints) = f (childNodePath path i) (unLoc located)
       in (x : xs, xHints <> hints)

addExprHint :: NodePath -> Parsed.Expr -> (Expr, Map NodePath LayoutHint) -> (Expr, Map NodePath LayoutHint)
addExprHint path parsedExpr = addHint path (exprLayout parsedExpr)

addPatHint :: NodePath -> Parsed.FuncPat -> (FuncPat, Map NodePath LayoutHint) -> (FuncPat, Map NodePath LayoutHint)
addPatHint path pat = addHint path (patLayout pat)

addHint :: NodePath -> Maybe ContainerLayout -> (a, Map NodePath LayoutHint) -> (a, Map NodePath LayoutHint)
addHint path mLayout (x, hints) = (x, maybe hints (\layout -> Map.insert path (LayoutHint (Just layout)) hints) mLayout)

wrapHints :: (Expr, Map NodePath LayoutHint) -> (Expr, LayoutHints)
wrapHints (expr, hints) = (expr, fold (fmap (uncurry singletonLayoutHint) (Map.toList hints)))

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

pureResult :: a -> (a, Map NodePath LayoutHint)
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
