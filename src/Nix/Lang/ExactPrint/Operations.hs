module Nix.Lang.ExactPrint.Operations
  ( -- * Cursor and gap operations
    RenderCursor (..),
    cursorAtSpanStart,
    cursorAtTokenStart,
    advanceCursor,
    renderGap,
    renderGapExact,
    renderGapText,
    renderGapFromOpen,
    renderGapFromOpenText,
    renderGapFromAnchor,
    renderGapFromAnchorText,
    renderGapToClose,
    renderGapToCloseText,
    renderGapFromDelta,
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
    mapTokenToDelta,
    deltaFromAnchor,

    -- * Type alias
    Expr,
    LExpr,
    Binding,
    LBinding,
    AttrPath,
    LAttrPath,
    AttrKey,
    LAttrKey,
    FuncPat,
    LFuncPat,
    SetPatBinding,
    LSetPatBinding,
    SetPatAs,
    LSetPatAs,
    Id,
    LId,
    Lit,
    LLit,
    Path,
    LPath,
    NString,
    LNString,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Annotation
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Prettyprinter (Doc, pretty)

--------------------------------------------------------------------------------

type Expr = NixExpr Ps

type LExpr = Located Expr

type Binding = NixBinding Ps

type LBinding = Located Binding

type AttrPath = NixAttrPath Ps

type LAttrPath = Located AttrPath

type AttrKey = NixAttrKey Ps

type LAttrKey = Located AttrKey

type FuncPat = NixFuncPat Ps

type LFuncPat = Located FuncPat

type SetPatBinding = NixSetPatBinding Ps

type LSetPatBinding = Located SetPatBinding

type SetPatAs = NixSetPatAs Ps

type LSetPatAs = Located SetPatAs

type Id = NixId Ps

type LId = Located Id

type Lit = NixLit Ps

type LLit = Located Lit

type Path = NixPath Ps

type LPath = Located Path

type NString = NixString Ps

type LNString = Located NString

--------------------------------------------------------------------------------

-- | Logical cursor used by exact-layout operations.
data RenderCursor = RenderCursor
  { rcLine :: Int,
    rcColumn :: Int
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

renderGap :: SrcSpan -> SrcSpan -> Doc ann
renderGap prev next = pretty (renderGapText True prev next)

renderGapExact :: SrcSpan -> SrcSpan -> Doc ann
renderGapExact prev next = pretty (renderGapText False prev next)

renderGapText :: Bool -> SrcSpan -> SrcSpan -> Text
renderGapText forceOne prev next
  | srcSpanFilename prev /= srcSpanFilename next = " "
  | srcSpanEndLine prev == srcSpanStartLine next =
      T.replicate (max minCols (srcSpanStartColumn next - srcSpanEndColumn prev)) " "
  | otherwise =
      T.replicate (max 1 (srcSpanStartLine next - srcSpanEndLine prev)) "\n"
        <> T.replicate (max 0 (srcSpanStartColumn next - 1)) " "
  where
    minCols = if forceOne then 1 else 0

renderGapFromOpen :: SrcSpan -> SrcSpan -> Doc ann
renderGapFromOpen openSpan next = pretty $ renderGapFromOpenText openSpan next

renderGapFromOpenText :: SrcSpan -> SrcSpan -> Text
renderGapFromOpenText openSpan next
  | srcSpanFilename openSpan /= srcSpanFilename next = " "
  | srcSpanStartLine openSpan == srcSpanStartLine next =
      T.replicate (max 0 (srcSpanStartColumn next - srcSpanStartColumn openSpan - 1)) " "
  | otherwise =
      T.replicate (max 1 (srcSpanStartLine next - srcSpanStartLine openSpan)) "\n"
        <> T.replicate (max 0 (srcSpanStartColumn next - 1)) " "

renderGapFromAnchor :: SrcSpan -> SrcSpan -> Doc ann
renderGapFromAnchor anchor next = pretty $ renderGapFromAnchorText anchor next

renderGapFromAnchorText :: SrcSpan -> SrcSpan -> Text
renderGapFromAnchorText anchor next
  | srcSpanFilename anchor /= srcSpanFilename next = " "
  | srcSpanStartLine anchor == srcSpanStartLine next =
      T.replicate (max 0 (srcSpanStartColumn next - srcSpanStartColumn anchor - anchorWidth anchor)) " "
  | otherwise =
      T.replicate (max 1 (srcSpanStartLine next - srcSpanStartLine anchor)) "\n"
        <> T.replicate (max 0 (srcSpanStartColumn next - 1)) " "

renderGapToClose :: SrcSpan -> SrcSpan -> Doc ann
renderGapToClose prev close = pretty $ renderGapToCloseText prev close

renderGapToCloseText :: SrcSpan -> SrcSpan -> Text
renderGapToCloseText prev close
  | srcSpanFilename prev /= srcSpanFilename close = ""
  | srcSpanEndLine prev == srcSpanStartLine close = ""
  | otherwise =
      T.replicate (max 1 (srcSpanStartLine close - srcSpanEndLine prev)) "\n"
        <> T.replicate (max 0 (srcSpanStartColumn close - 1)) " "

renderGapFromDelta :: DeltaPos -> Doc ann
renderGapFromDelta = pretty . renderGapFromDeltaText

--------------------------------------------------------------------------------

cursorAtSpanStart :: SrcSpan -> RenderCursor
cursorAtSpanStart src = RenderCursor (srcSpanStartLine src) (srcSpanStartColumn src)

cursorAtTokenStart :: SrcSpan -> RenderCursor
cursorAtTokenStart = cursorAtSpanStart

advanceCursor :: RenderCursor -> Text -> RenderCursor
advanceCursor cursor txt = foldl step cursor (T.unpack txt)
  where
    step RenderCursor {..} ch = if ch == '\n' then RenderCursor (rcLine + 1) 1 else RenderCursor rcLine (rcColumn + 1)

--------------------------------------------------------------------------------

renderGapFromCursorToSpan :: RenderCursor -> SrcSpan -> Doc ann
renderGapFromCursorToSpan cursor span' = pretty (renderGapFromCursorToSpanText cursor span')

renderGapFromCursorToSpanText :: RenderCursor -> SrcSpan -> Text
renderGapFromCursorToSpanText RenderCursor {..} next
  | rcLine == srcSpanStartLine next = T.replicate (max 0 (srcSpanStartColumn next - rcColumn)) " "
  | otherwise = T.replicate (max 1 (srcSpanStartLine next - rcLine)) "\n" <> T.replicate (max 0 (srcSpanStartColumn next - 1)) " "

renderGapFromDeltaText :: DeltaPos -> Text
renderGapFromDeltaText DeltaPos {..}
  | deltaLine <= 0 = T.replicate deltaColumn " "
  | otherwise = T.replicate deltaLine "\n" <> T.replicate deltaColumn " "

anchorWidth :: SrcSpan -> Int
anchorWidth anchorSpan
  | srcSpanStartLine anchorSpan == srcSpanEndLine anchorSpan = max 1 (srcSpanEndColumn anchorSpan - srcSpanStartColumn anchorSpan)
  | otherwise = 1

--------------------------------------------------------------------------------

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

fallbackExprSpan :: Maybe SrcSpan -> [SrcSpan] -> SrcSpan
fallbackExprSpan maybeAnnSpan childSpans =
  case maybeAnnSpan of
    Just span' -> span'
    Nothing -> foldr1 combineSrcSpans childSpans

--------------------------------------------------------------------------------

funcPatBodySpan :: FuncPat -> SrcSpan
funcPatBodySpan = \case
  NixVarPat _ ident -> getLoc ident
  NixSetPat ann _ mAs bindings ->
    let base = expectTokenSpan "set pattern open" (aspOpenC ann) `combineSrcSpans` expectTokenSpan "set pattern close" (aspCloseC ann)
        withAs = maybe base (combineSrcSpans base . getLoc) mAs
     in foldr (combineSrcSpans . getLoc) withAs bindings

--------------------------------------------------------------------------------

funcPatRenderSpan :: FuncPat -> SrcSpan
funcPatRenderSpan pat = foldr combineSrcSpans (funcPatBodySpan pat) (getLoc <$> priorComments (funcPatComments pat))

funcPatComments :: FuncPat -> NodeComments
funcPatComments = \case
  NixVarPat ann _ -> annComments ann
  NixSetPat ann _ _ _ -> annComments ann

--------------------------------------------------------------------------------

attrPathRenderSpan :: AttrPath -> SrcSpan
attrPathRenderSpan path@(NixAttrPath ann _) = foldr combineSrcSpans (attrPathSpan path) (getLoc <$> priorComments (annComments ann))

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

--------------------------------------------------------------------------------

expectTokenSpan :: Text -> AnnToken -> SrcSpan
expectTokenSpan label tok = fromMaybe (error (T.unpack label <> ": missing token span")) (annTokenSrcSpan tok)

aspaAtSpan :: AnnSetPatAs -> SrcSpan -> SrcSpan
aspaAtSpan ann anchor = fromMaybe anchor (annTokenSrcSpan (aspaAt ann))

--------------------------------------------------------------------------------

setPatAsRenderSpan :: AnnSetPatAs -> LId -> NixSetPatAsLocation -> SrcSpan
setPatAsRenderSpan ann var loc =
  case loc of
    NixSetPatAsLeading -> getLoc var `combineSrcSpans` aspaAtSpan ann (getLoc var)
    NixSetPatAsTrailing -> aspaAtSpan ann (getLoc var) `combineSrcSpans` getLoc var

--------------------------------------------------------------------------------

prepareListLayout :: AnnListNode -> [LExpr] -> AnnListNode
prepareListLayout ann xs = ann {alnCloseS = closeToken}
  where
    closeToken = case annTokenSrcSpan (alnCloseS ann) of
      Just closeSpan -> mapTokenToDelta (closeDelta closeSpan) (alnCloseS ann)
      Nothing -> alnCloseS ann
    closeDelta closeSpan = case xs of
      [] -> case annTokenSrcSpan (alnOpenS ann) of
        Just openSpan -> deltaFromAnchor openSpan closeSpan
        Nothing -> DeltaPos 0 0
      _ -> deltaFromAnchor (getLoc (last xs)) closeSpan

--------------------------------------------------------------------------------

mapTokenToDelta :: DeltaPos -> AnnToken -> AnnToken
mapTokenToDelta delta tok = tok {annTokenPos = AnnDelta delta}

--------------------------------------------------------------------------------

prepareParLayout :: AnnParNode -> Expr -> AnnParNode
prepareParLayout ann expr = ann {apnCloseP = closeToken}
  where
    closeToken = case annTokenSrcSpan (apnCloseP ann) of
      Just closeSpan
        | srcSpanEndLine (exprSpan expr) < srcSpanStartLine closeSpan -> mapTokenToDelta (deltaFromAnchor (exprSpan expr) closeSpan) (apnCloseP ann)
        | otherwise -> apnCloseP ann
      Nothing -> apnCloseP ann

--------------------------------------------------------------------------------

prepareSetLayout :: AnnSet -> [LBinding] -> AnnSet
prepareSetLayout ann bindings = ann {asCloseC = closeToken}
  where
    closeToken = case annTokenSrcSpan (asCloseC ann) of
      Just closeSpan ->
        let anchor = case bindings of
              [] -> maybe closeSpan id (annTokenSrcSpan =<< (asRec ann <|> Just (asOpenC ann)))
              _ -> getLoc (last bindings)
         in mapTokenToDelta (deltaFromAnchor anchor closeSpan) (asCloseC ann)
      Nothing -> asCloseC ann

--------------------------------------------------------------------------------

prepareLetLayout :: AnnLetNode -> [LBinding] -> Expr -> AnnLetNode
prepareLetLayout ann bindings _ = ann {alIn = inTok}
  where
    inTok = case annTokenSrcSpan (alIn ann) of
      Just inSpan ->
        let anchor = case bindings of
              [] -> maybe inSpan id (annTokenSrcSpan (alLet ann))
              _ -> getLoc (last bindings)
         in mapTokenToDelta (deltaFromAnchor anchor inSpan) (alIn ann)
      Nothing -> alIn ann

--------------------------------------------------------------------------------

prepareIfLayout :: AnnIfNode -> Expr -> Expr -> Expr -> AnnIfNode
prepareIfLayout ann cond thenExpr _ = ann {aifThen = thenTok, aifElse = elseTok}
  where
    thenTok = case annTokenSrcSpan (aifThen ann) of
      Just thenSpan -> mapTokenToDelta (deltaFromAnchor (exprSpan cond) thenSpan) (aifThen ann)
      Nothing -> aifThen ann
    elseTok = case annTokenSrcSpan (aifElse ann) of
      Just elseSpan -> mapTokenToDelta (deltaFromAnchor (exprSpan thenExpr) elseSpan) (aifElse ann)
      Nothing -> aifElse ann

--------------------------------------------------------------------------------

prepareWithLayout :: AnnWithNode -> Expr -> Expr -> AnnWithNode
prepareWithLayout ann scope _ = ann {awSemicolon = semTok}
  where
    semTok = case annTokenSrcSpan (awSemicolon ann) of
      Just semSpan -> mapTokenToDelta (deltaFromAnchor (exprSpan scope) semSpan) (awSemicolon ann)
      Nothing -> awSemicolon ann

--------------------------------------------------------------------------------

prepareAssertLayout :: AnnAssertNode -> Expr -> Expr -> AnnAssertNode
prepareAssertLayout ann assertion _ = ann {aaSemicolon = semTok}
  where
    semTok = case annTokenSrcSpan (aaSemicolon ann) of
      Just semSpan -> mapTokenToDelta (deltaFromAnchor (exprSpan assertion) semSpan) (aaSemicolon ann)
      Nothing -> aaSemicolon ann

--------------------------------------------------------------------------------

prepareHasAttrLayout :: AnnHasAttr -> Expr -> AttrPath -> AnnHasAttr
prepareHasAttrLayout ann expr _ = ann {ahaQuestion = qTok}
  where
    qTok = case annTokenSrcSpan (ahaQuestion ann) of
      Just qSpan -> mapTokenToDelta (deltaFromAnchor (exprSpan expr) qSpan) (ahaQuestion ann)
      Nothing -> ahaQuestion ann

--------------------------------------------------------------------------------

prepareSelectLayout :: AnnSelect -> Expr -> AttrPath -> Maybe LExpr -> AnnSelect
prepareSelectLayout ann _ path def = ann {aslOr = orTok}
  where
    orTok = case (aslOr ann, def) of
      (Just tok, Just _) -> case annTokenSrcSpan tok of
        Just orSpan -> Just $ mapTokenToDelta (deltaFromAnchor (attrPathSpan path) orSpan) tok
        Nothing -> Just tok
      _ -> aslOr ann

--------------------------------------------------------------------------------

deltaFromAnchor :: SrcSpan -> SrcSpan -> DeltaPos
deltaFromAnchor anchor target
  | srcSpanFilename anchor /= srcSpanFilename target = DeltaPos 0 0
  | srcSpanEndLine anchor == srcSpanStartLine target = DeltaPos 0 (max 0 (srcSpanStartColumn target - srcSpanEndColumn anchor))
  | otherwise = DeltaPos (max 1 (srcSpanStartLine target - srcSpanEndLine anchor)) (max 0 (srcSpanStartColumn target - 1))
