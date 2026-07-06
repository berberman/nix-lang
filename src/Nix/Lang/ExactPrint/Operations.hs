-- | Low-level cursor, span, and token operations for exact printing.
--
-- This is the low-level geometry layer under the exact printer: cursor movement,
-- token spans, whitespace gaps, and token retagging after subtree repair.
--
-- It knows how to:
--
-- * render gaps between already-anchored spans,
-- * recover structural spans from annotated nodes,
-- * convert absolute token spans back into relative deltas,
-- * and retag layout-sensitive tokens after subtree repair.
module Nix.Lang.ExactPrint.Operations
  ( -- * Cursor and gap operations
    RenderCursor (..),
    cursorAtSpanStart,
    cursorAtTokenStart,
    advanceCursor,
    renderGapFromDeltaText,
    renderGapFromCursorToSpan,
    renderGapFromCursorToSpanText,

    -- * Span and token queries
    exprSpan,
    funcPatBodySpan,
    funcPatRenderSpan,
    attrPathRenderSpan,
    attrPathSpan,
    expectTokenSpan,
    aspaAtSpan,
    setPatAsRenderSpan,

    -- * Layout repair helpers
    prepareListLayout,
    prepareParLayout,
    prepareSetLayout,
    prepareLetLayout,
    prepareIfLayout,
    prepareWithLayout,
    prepareAssertLayout,
    prepareHasAttrLayout,
    prepareSelectLayout,
    deltaFromAnchor,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Annotation
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Types.Ps
import Nix.Lang.Utils
import Prettyprinter (Doc, pretty)

--------------------------------------------------------------------------------

-- | Logical cursor used by exact-layout operations.
data RenderCursor = RenderCursor
  { rcLine :: !Int,
    rcColumn :: !Int
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | Cursor at the start of a span.
cursorAtSpanStart :: SrcSpan -> RenderCursor
cursorAtSpanStart src = RenderCursor (srcSpanStartLine src) (srcSpanStartColumn src)

-- | Cursor at the start of a token span.
cursorAtTokenStart :: SrcSpan -> RenderCursor
cursorAtTokenStart = cursorAtSpanStart

-- | Advance a render cursor through concrete output text.
advanceCursor :: RenderCursor -> Text -> RenderCursor
advanceCursor cursor = T.foldl' step cursor
  where
    step RenderCursor {..} ch = if ch == '\n' then RenderCursor (rcLine + 1) 1 else RenderCursor rcLine (rcColumn + 1)

--------------------------------------------------------------------------------

-- | Render the gap from a cursor to a span.
renderGapFromCursorToSpan :: RenderCursor -> SrcSpan -> Doc ann
renderGapFromCursorToSpan cursor span' = pretty (renderGapFromCursorToSpanText cursor span')

-- | Compute the textual gap from a cursor to a span.
renderGapFromCursorToSpanText :: RenderCursor -> SrcSpan -> Text
renderGapFromCursorToSpanText RenderCursor {..} next
  | rcLine == srcSpanStartLine next = T.replicate (max 0 (srcSpanStartColumn next - rcColumn)) " "
  | otherwise = T.replicate (max 1 (srcSpanStartLine next - rcLine)) "\n" <> T.replicate (max 0 (srcSpanStartColumn next - 1)) " "

-- | Render whitespace described by a relative token delta.
renderGapFromDeltaText :: DeltaPos -> Text
renderGapFromDeltaText DeltaPos {..}
  | deltaLine <= 0 = T.replicate deltaColumn " "
  | otherwise = T.replicate deltaLine "\n" <> T.replicate deltaColumn " "

--------------------------------------------------------------------------------

-- | Recover the concrete span of an expression.
--
-- This prefers token-backed spans where possible and falls back to annotation or
-- child spans when an exact token span is missing.
exprSpan :: Expr -> SrcSpan
exprSpan = \case
  NixVar _ ident -> getLoc ident
  NixLit _ lit -> getLoc lit
  NixPar ann expr -> fromMaybe (fallbackExprSpan (annSrcSpan ann) [getLoc expr]) $ do
    openSpan <- annTokenSrcSpan (apnOpenP ann)
    closeSpan <- annTokenSrcSpan (apnCloseP ann)
    pure (openSpan `combineSrcSpans` getLoc expr `combineSrcSpans` closeSpan)
  NixString _ str -> getLoc str
  NixPath _ path -> getLoc path
  NixEnvPath ann path -> fromMaybe (fallbackExprSpan (annSrcSpan ann) [getLoc path]) $ do
    openSpan <- annTokenSrcSpan (aenvOpen ann)
    closeSpan <- annTokenSrcSpan (aenvClose ann)
    pure (openSpan `combineSrcSpans` getLoc path `combineSrcSpans` closeSpan)
  NixLam ann pat body -> foldr combineSrcSpans (funcPatRenderSpan (unLoc pat) `combineSrcSpans` getLoc body) (getLoc <$> priorComments (annComments ann))
  NixApp _ lhs rhs -> getLoc lhs `combineSrcSpans` getLoc rhs
  NixBinApp _ _ lhs rhs -> getLoc lhs `combineSrcSpans` getLoc rhs
  NixNotApp ann expr -> maybe (fallbackExprSpan (annSrcSpan ann) [getLoc expr]) (`combineSrcSpans` getLoc expr) (annTokenSrcSpan (apfxToken ann))
  NixNegApp ann expr -> maybe (fallbackExprSpan (annSrcSpan ann) [getLoc expr]) (`combineSrcSpans` getLoc expr) (annTokenSrcSpan (apfxToken ann))
  NixList ann xs ->
    fromMaybe (fallbackExprSpan (annSrcSpan ann) (getLoc <$> xs)) $ do
      openSpan <- annTokenSrcSpan (alnOpenS ann)
      closeSpan <- annTokenSrcSpan (alnCloseS ann)
      pure (foldr combineSrcSpans (openSpan `combineSrcSpans` closeSpan) (getLoc <$> xs))
  NixSet ann _ bindings ->
    fromMaybe (fallbackExprSpan (annSrcSpan ann) [getLoc bindings]) $ do
      openSpan <- maybe (annTokenSrcSpan (asOpenC ann)) annTokenSrcSpan (asRec ann)
      closeSpan <- annTokenSrcSpan (asCloseC ann)
      pure (openSpan `combineSrcSpans` getLoc bindings `combineSrcSpans` closeSpan)
  NixLet ann bindings expr ->
    fromMaybe (fallbackExprSpan (annSrcSpan ann) [getLoc bindings, getLoc expr]) $ do
      letSpan <- annTokenSrcSpan (alLet ann)
      inSpan <- annTokenSrcSpan (alIn ann)
      pure (letSpan `combineSrcSpans` getLoc bindings `combineSrcSpans` inSpan `combineSrcSpans` getLoc expr)
  NixHasAttr ann expr path ->
    maybe (fallbackExprSpan (annSrcSpan ann) [getLoc expr, getLoc path]) (getLoc expr `combineSrcSpans`) (annTokenSrcSpan (ahaQuestion ann))
      `combineSrcSpans` getLoc path
  NixSelect _ expr path def -> maybe (getLoc expr `combineSrcSpans` getLoc path) ((getLoc expr `combineSrcSpans` getLoc path) `combineSrcSpans`) (getLoc <$> def)
  NixIf ann cond thenExpr elseExpr ->
    fromMaybe (fallbackExprSpan (annSrcSpan ann) [getLoc cond, getLoc thenExpr, getLoc elseExpr]) $ do
      ifSpan <- annTokenSrcSpan (aifIf ann)
      thenSpan <- annTokenSrcSpan (aifThen ann)
      elseSpan <- annTokenSrcSpan (aifElse ann)
      pure (ifSpan `combineSrcSpans` getLoc cond `combineSrcSpans` thenSpan `combineSrcSpans` getLoc thenExpr `combineSrcSpans` elseSpan `combineSrcSpans` getLoc elseExpr)
  NixWith ann scope expr ->
    fromMaybe (fallbackExprSpan (annSrcSpan ann) [getLoc scope, getLoc expr]) $ do
      withSpan <- annTokenSrcSpan (awWith ann)
      semiSpan <- annTokenSrcSpan (awSemicolon ann)
      pure (withSpan `combineSrcSpans` getLoc scope `combineSrcSpans` semiSpan `combineSrcSpans` getLoc expr)
  NixAssert ann assertion expr ->
    fromMaybe (fallbackExprSpan (annSrcSpan ann) [getLoc assertion, getLoc expr]) $ do
      assertSpan <- annTokenSrcSpan (aaAssert ann)
      semiSpan <- annTokenSrcSpan (aaSemicolon ann)
      pure (assertSpan `combineSrcSpans` getLoc assertion `combineSrcSpans` semiSpan `combineSrcSpans` getLoc expr)

-- | Fallback span computation for expressions when token spans are missing.
fallbackExprSpan :: Maybe SrcSpan -> [SrcSpan] -> SrcSpan
fallbackExprSpan maybeAnnSpan childSpans =
  case maybeAnnSpan of
    Just span' -> span'
    Nothing -> foldr1 combineSrcSpans childSpans

-- | Recover the concrete body span of a function pattern.
funcPatBodySpan :: FuncPat -> SrcSpan
funcPatBodySpan = \case
  NixVarPat _ ident -> getLoc ident
  NixSetPat ann _ mAs bindings ->
    let base = expectTokenSpan "set pattern open" (aspOpenC ann) `combineSrcSpans` expectTokenSpan "set pattern close" (aspCloseC ann)
        withAs = maybe base (combineSrcSpans base . getLoc) mAs
     in foldr (combineSrcSpans . getLoc) withAs bindings

-- | Extend 'funcPatBodySpan' to include leading comments owned by the pattern.
funcPatRenderSpan :: FuncPat -> SrcSpan
funcPatRenderSpan pat = foldr combineSrcSpans (funcPatBodySpan pat) (getLoc <$> priorComments (funcPatComments pat))

-- | Get the comment payload owned by a function pattern.
funcPatComments :: FuncPat -> NodeComments
funcPatComments = \case
  NixVarPat ann _ -> annComments ann
  NixSetPat ann _ _ _ -> annComments ann

-- | Extend 'attrPathSpan' to include leading comments owned by the path.
attrPathRenderSpan :: AttrPath -> SrcSpan
attrPathRenderSpan path@(NixAttrPath ann _) = foldr combineSrcSpans (attrPathSpan path) (getLoc <$> priorComments (annComments ann))

-- | Recover the concrete span of an attribute path.
--
-- Leading-dot forms are anchored from the first dot token when present; other
-- forms start at the first key.
attrPathSpan :: AttrPath -> SrcSpan
attrPathSpan (NixAttrPath ann keys) =
  foldr combineSrcSpans base (getLoc <$> keys)
  where
    base = case aapDots ann of
      dotTok : _ -> fromMaybe keyBase (annTokenSrcSpan dotTok)
      [] -> case keys of
        key : _ -> getLoc key
        [] -> error "attrPathSpan: empty attribute path"
    keyBase = case keys of
      key : _ -> getLoc key
      [] -> error "attrPathSpan: empty attribute path"

-- | Recover the render span of a set-pattern @as@ binding.
setPatAsRenderSpan :: AnnSetPatAs -> LBinderName -> NixSetPatAsLocation -> SrcSpan
setPatAsRenderSpan ann var loc =
  case loc of
    NixSetPatAsLeading -> getLoc var `combineSrcSpans` aspaAtSpan ann (getLoc var)
    NixSetPatAsTrailing -> aspaAtSpan ann (getLoc var) `combineSrcSpans` getLoc var

--------------------------------------------------------------------------------

-- | Read a token span or fail loudly when an invariant is broken.
--
-- Exact-print repair expects certain tokens to remain present. This helper makes
-- those assumptions explicit and gives failures a label.
expectTokenSpan :: Text -> AnnToken -> SrcSpan
expectTokenSpan label tok = fromMaybe (error (T.unpack label <> ": missing token span")) (annTokenSrcSpan tok)

-- | Recover the span of the @at@ token in a set-pattern @as@ binding.
aspaAtSpan :: AnnSetPatAs -> SrcSpan -> SrcSpan
aspaAtSpan ann anchor = fromMaybe anchor (annTokenSrcSpan (aspaAt ann))

--------------------------------------------------------------------------------

-- | Retag the close token of a list after its elements have been repaired.
prepareListLayout :: AnnListNode -> [LExpr] -> AnnListNode
prepareListLayout ann xs = ann {alnCloseS = closeToken}
  where
    -- if the lisst is empty, anchor ] to [
    -- otherwise anchor ] to the last element
    closeToken = rewriteToken (alnCloseS ann) closeDelta
    closeDelta closeSpan = case xs of
      [] -> case annTokenSrcSpan (alnOpenS ann) of
        Just openSpan -> deltaFromAnchor openSpan closeSpan
        Nothing -> DeltaPos 0 0
      _ -> deltaFromAnchor (getLoc (last xs)) closeSpan

--------------------------------------------------------------------------------

-- | Re-anchor a token relative to a concrete span.
anchorToken :: SrcSpan -> AnnToken -> AnnToken
anchorToken anchor tok = rewriteToken tok (deltaFromAnchor anchor)

-- | Re-anchor an optional token relative to a concrete span.
anchorMaybeToken :: SrcSpan -> Maybe AnnToken -> Maybe AnnToken
anchorMaybeToken anchor = fmap (anchorToken anchor)

-- | Rewrite a token by deriving a new delta from its current concrete span.
-- If the token has a concrete span, convert it to a delta; otherwise, leave it unchanged.
rewriteToken :: AnnToken -> (SrcSpan -> DeltaPos) -> AnnToken
rewriteToken tok mkDelta =
  case annTokenSrcSpan tok of
    Just span' -> setAnnTokenDelta (mkDelta span') tok
    Nothing -> tok

--------------------------------------------------------------------------------

-- | Retag the close token of a parenthesized expression.
prepareParLayout :: AnnParNode -> Expr -> AnnParNode
prepareParLayout ann expr = ann {apnCloseP = closeToken}
  where
    closeToken = rewriteToken (apnCloseP ann) $ \closeSpan ->
      if srcSpanEndLine (exprSpan expr) < srcSpanStartLine closeSpan
        then deltaFromAnchor (exprSpan expr) closeSpan
        else tokenDeltaOrZero (apnCloseP ann)

--------------------------------------------------------------------------------

-- | Retag the close token of a set after its bindings have been repaired.
prepareSetLayout :: AnnSet -> [LBinding] -> AnnSet
prepareSetLayout ann bindings = ann {asCloseC = closeToken}
  where
    closeToken = rewriteToken (asCloseC ann) $ \closeSpan ->
      let anchor = case bindings of
            [] -> maybe closeSpan id (annTokenSrcSpan =<< (asRec ann <|> Just (asOpenC ann)))
            _ -> getLoc (last bindings)
       in deltaFromAnchor anchor closeSpan

--------------------------------------------------------------------------------

-- | Retag the @in@ token of a @let@ after its bindings have been repaired.
prepareLetLayout :: AnnLetNode -> [LBinding] -> Expr -> AnnLetNode
prepareLetLayout ann bindings _ = ann {alIn = inTok}
  where
    -- if there are bindings, anchor in to the last binding
    -- otherwise anchor in to let
    inTok = rewriteToken (alIn ann) $ \inSpan ->
      let anchor = case bindings of
            [] -> maybe inSpan id (annTokenSrcSpan (alLet ann))
            _ -> getLoc (last bindings)
       in deltaFromAnchor anchor inSpan

--------------------------------------------------------------------------------

-- | Retag the @then@ and @else@ tokens after repairing an @if@ chain.
prepareIfLayout :: AnnIfNode -> Expr -> Expr -> Expr -> AnnIfNode
prepareIfLayout ann cond thenExpr _ = ann {aifThen = thenTok, aifElse = elseTok}
  where
    -- anchor then to the condition span
    thenTok = anchorToken (exprSpan cond) (aifThen ann)
    -- anchor else to the then expression span
    elseTok = anchorToken (exprSpan thenExpr) (aifElse ann)

--------------------------------------------------------------------------------

-- | Retag the semicolon in a @with@ expression after repairing its scope.
prepareWithLayout :: AnnWithNode -> Expr -> Expr -> AnnWithNode
prepareWithLayout ann scope _ = ann {awSemicolon = semTok}
  where
    -- anchor ; to the scope span
    semTok = anchorToken (exprSpan scope) (awSemicolon ann)

--------------------------------------------------------------------------------

-- | Retag the semicolon in an @assert@ expression after repairing its assertion.
prepareAssertLayout :: AnnAssertNode -> Expr -> Expr -> AnnAssertNode
prepareAssertLayout ann assertion _ = ann {aaSemicolon = semTok}
  where
    -- anchor ; to the assertion span
    semTok = anchorToken (exprSpan assertion) (aaSemicolon ann)

--------------------------------------------------------------------------------

-- | Retag the question-mark token in a @hasAttr@ expression.
prepareHasAttrLayout :: AnnHasAttr -> Expr -> AttrPath -> AnnHasAttr
prepareHasAttrLayout ann expr _ = ann {ahaQuestion = qTok}
  where
    -- anchor ? to the expression span
    qTok = anchorToken (exprSpan expr) (ahaQuestion ann)

--------------------------------------------------------------------------------

-- | Retag the optional @or@ token in a selection expression.
prepareSelectLayout :: AnnSelect -> Expr -> AttrPath -> Maybe LExpr -> AnnSelect
prepareSelectLayout ann _ path def = ann {aslOr = orTok}
  where
    -- if or is present, anchor it to the attribute path span
    orTok = case def of
      Just _ -> anchorMaybeToken (attrPathSpan path) (aslOr ann)
      Nothing -> aslOr ann

--------------------------------------------------------------------------------

-- | Read a token's stored delta, defaulting to zero when it has no delta.
tokenDeltaOrZero :: AnnToken -> DeltaPos
tokenDeltaOrZero = fromMaybe (DeltaPos 0 0) . annTokenDelta

-- | Compute the relative delta from one anchor span to a target span.
deltaFromAnchor :: SrcSpan -> SrcSpan -> DeltaPos
deltaFromAnchor anchor target
  | srcSpanFilename anchor /= srcSpanFilename target = DeltaPos 0 0
  | srcSpanEndLine anchor == srcSpanStartLine target = DeltaPos 0 (max 0 (srcSpanStartColumn target - srcSpanEndColumn anchor))
  | otherwise = DeltaPos (max 1 (srcSpanStartLine target - srcSpanEndLine anchor)) (max 0 (srcSpanStartColumn target - 1))
