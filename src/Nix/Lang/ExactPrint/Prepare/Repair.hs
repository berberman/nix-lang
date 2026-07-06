-- | Recursive repair engine for exact-print normalization.
--
-- After an edit changes tree shape, this module recalculates spans, token
-- deltas, and related annotations so the tree can be rendered again.
module Nix.Lang.ExactPrint.Prepare.Repair
  ( repairExprLayout,
    repairBindingLayout,
    repairAttrPathLayout,
    repairFuncPatLayout,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT (..), ask, local, runReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint.Prepare.Rebuild
import Nix.Lang.ExactPrint.Prepare.Types
import Nix.Lang.ExactPrint.Prepare.Utils
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Outputable (renderToText)
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Types.Ps
import Nix.Lang.Utils

--------------------------------------------------------------------------------

newtype RepairContext = RepairContext
  { repairCursor :: RenderCursor
  }

type RepairM = ReaderT RepairContext EPResult

--------------------------------------------------------------------------------

runRepairAt :: RenderCursor -> RepairM a -> EPResult a
runRepairAt cursor = flip runReaderT (RepairContext cursor)

withRepairCursor :: RenderCursor -> RepairM a -> RepairM a
withRepairCursor cursor = local (\ctx -> ctx {repairCursor = cursor})

currentRepairCursor :: RepairM RenderCursor
currentRepairCursor = repairCursor <$> ask

--------------------------------------------------------------------------------

repairExprLayout :: Expr -> EPResult Expr
repairExprLayout = repairLocatedLayout exprSpan repairExpr

repairBindingLayout :: Binding -> EPResult Binding
repairBindingLayout = repairLocatedLayout bindingSpan repairBinding

repairAttrPathLayout :: AttrPath -> EPResult AttrPath
repairAttrPathLayout = repairLocatedLayout attrPathSpan repairAttrPath

repairFuncPatLayout :: FuncPat -> EPResult FuncPat
repairFuncPatLayout = repairLocatedLayout funcPatBodySpan repairFuncPat

--------------------------------------------------------------------------------

repairLocatedLayout :: (a -> SrcSpan) -> (Located a -> RepairM (Located a)) -> a -> EPResult a
repairLocatedLayout spanOf repair value =
  unLoc <$> runRepairAt (spanStartCursor span') (repair (L span' value))
  where
    span' = spanOf value

--------------------------------------------------------------------------------

-- | Repair an expression subtree starting from the current repair cursor.
repairExpr :: LExpr -> RepairM LExpr
repairExpr (L originalSpan node) =
  case node of
    NixVar ann ident -> pure (repairVarExpr originalSpan ann ident)
    NixLit ann lit -> pure (repairLit originalSpan ann lit)
    NixPar ann inner -> repairPar originalSpan ann inner
    NixString ann str -> pure (repairStringExpr originalSpan ann str)
    NixPath ann path -> pure (repairPathExpr originalSpan ann path)
    NixEnvPath ann path -> pure (repairEnvPath originalSpan ann path)
    NixLam ann pat body -> repairLam originalSpan ann pat body
    NixApp ann lhs rhs -> repairApp originalSpan ann lhs rhs
    NixBinApp ann op lhs rhs -> repairBinApp originalSpan ann op lhs rhs
    NixNotApp ann inner -> repairPrefix originalSpan ann NixNotApp inner
    NixNegApp ann inner -> repairPrefix originalSpan ann NixNegApp inner
    NixList ann xs -> repairList ann xs
    NixSet ann kind (L _ bindings) -> repairSet ann kind bindings
    NixLet ann (L _ bindings) body -> repairLet ann bindings body
    NixHasAttr ann lhs path -> repairHasAttr originalSpan ann lhs path
    NixSelect ann lhs path def -> repairSelect originalSpan ann lhs path def
    NixIf ann cond thenExpr elseExpr -> repairIf originalSpan ann cond thenExpr elseExpr
    NixWith ann scope body -> repairWith originalSpan ann scope body
    NixAssert ann assertion body -> repairAssert originalSpan ann assertion body

repairExprAt :: RenderCursor -> LExpr -> RepairM LExpr
repairExprAt cursor expr = withRepairCursor cursor (repairExpr translated)
  where
    translated = translateFromTo (getLoc expr) (cursorSpan cursor (srcSpanFilename (getLoc expr))) expr

repairChildAfter :: SrcSpan -> SrcSpan -> SrcSpan -> LExpr -> RepairM LExpr
repairChildAfter oldAnchor oldTarget newAnchor child =
  repairExprAt (preserveGapTarget oldAnchor oldTarget newAnchor) child

--------------------------------------------------------------------------------

repairVarExpr :: SrcSpan -> AnnCommon -> LVarName -> LExpr
repairVarExpr originalSpan ann ident =
  let ident' = repairLocatedTextAt (spanStartCursor originalSpan) ident
      span' = getLoc ident'
      ann' = setAnnSpan span' ann
   in L span' (NixVar ann' ident')

--------------------------------------------------------------------------------

repairLit :: SrcSpan -> AnnCommon -> LLit -> LExpr
repairLit originalSpan ann lit =
  let lit' = repairLocatedLitAt (spanStartCursor originalSpan) lit
      span' = getLoc lit'
      ann' = setAnnSpan span' ann
   in L span' (NixLit ann' lit')

--------------------------------------------------------------------------------

repairStringExpr :: SrcSpan -> AnnStringNode -> LNString -> LExpr
repairStringExpr originalSpan ann str =
  let str' = repairLocatedStringAt (spanStartCursor originalSpan) str
      span' = getLoc str'
      ann' = setAnnSpan span' ann
   in L span' (NixString ann' str')

--------------------------------------------------------------------------------

repairPathExpr :: SrcSpan -> AnnPathNode -> LPath -> LExpr
repairPathExpr originalSpan ann path =
  let path' = repairLocatedPathAt (spanStartCursor originalSpan) path
      span' = getLoc path'
      ann' = setAnnSpan span' ann
   in L span' (NixPath ann' path')

--------------------------------------------------------------------------------

repairPar :: SrcSpan -> AnnParNode -> LExpr -> RepairM LExpr
repairPar originalSpan ann inner = do
  let openSpan = tokenSpanAt (spanStartCursor originalSpan) (apnOpenP ann)
  inner' <- repairChildAfter (expectTokenSpan "paren open" (apnOpenP ann)) (getLoc inner) openSpan inner
  let closeSpan = preserveGapSpan (getLoc inner) (tokenSpanFromAnchor "paren close" (getLoc inner') (apnCloseP ann)) inner'
      ann0 = ann {apnOpenP = (apnOpenP ann) {annTokenPos = AnnSpan openSpan}, apnCloseP = (apnCloseP ann) {annTokenPos = AnnSpan closeSpan}}
      ann' = setAnnSpan (openSpan `combineSrcSpans` closeSpan) (prepareParLayout ann0 (unLoc inner'))
      expr' = NixPar ann' inner'
  pure (L (exprSpan expr') expr')

--------------------------------------------------------------------------------

repairEnvPath :: SrcSpan -> AnnEnvPathNode -> Located Text -> LExpr
repairEnvPath _ ann path =
  let openSpan = expectTokenSpan "env path open" (aenvOpen ann)
      path' = repairLocatedTextAt (spanStartCursor (preserveGapSpan openSpan (getLoc path) (L openSpan ()))) path
      closeSpan = preserveGapSpan (getLoc path) (expectTokenSpan "env path close" (aenvClose ann)) path'
      ann' = setAnnSpan (openSpan `combineSrcSpans` closeSpan) ann {aenvClose = (aenvClose ann) {annTokenPos = AnnSpan closeSpan}}
      span' = openSpan `combineSrcSpans` closeSpan
   in L span' (NixEnvPath ann' path')

--------------------------------------------------------------------------------

repairLam :: SrcSpan -> AnnLamNode -> LFuncPat -> LExpr -> RepairM LExpr
repairLam originalSpan ann pat body = do
  pat' <- repairFuncPatAt (spanStartCursor originalSpan) pat
  let colonSpan = preserveGapSpan (funcPatRenderSpan (unLoc pat)) (expectTokenSpan "lambda colon" (alamColon ann)) pat'
  body' <- repairChildAfter (expectTokenSpan "lambda colon" (alamColon ann)) (getLoc body) colonSpan body
  let span' = funcPatRenderSpan (unLoc pat') `combineSrcSpans` getLoc body'
      ann' = setAnnSpan span' ann {alamColon = (alamColon ann) {annTokenPos = AnnSpan colonSpan}}
  pure (L span' (NixLam ann' pat' body'))

--------------------------------------------------------------------------------

repairApp :: SrcSpan -> AnnAppNode -> LExpr -> LExpr -> RepairM LExpr
repairApp originalSpan ann lhs rhs = do
  lhs' <- repairExprAt (spanStartCursor originalSpan) lhs
  rhs' <- repairChildAfter (getLoc lhs) (getLoc rhs) (getLoc lhs') rhs
  let span' = getLoc lhs' `combineSrcSpans` getLoc rhs'
      ann' = setAnnSpan span' ann
  pure (L span' (NixApp ann' lhs' rhs'))

--------------------------------------------------------------------------------

repairBinApp :: SrcSpan -> AnnBinAppNode -> BinaryOp -> LExpr -> LExpr -> RepairM LExpr
repairBinApp originalSpan ann op lhs rhs = do
  lhs' <- repairExprAt (spanStartCursor originalSpan) lhs
  let opSpan = preserveGapSpan (getLoc lhs) (expectTokenSpan "binary operator" (abinOperator ann)) lhs'
  rhs' <- repairChildAfter (expectTokenSpan "binary operator" (abinOperator ann)) (getLoc rhs) opSpan rhs
  let span' = getLoc lhs' `combineSrcSpans` getLoc rhs'
      ann' = setAnnSpan span' ann {abinOperator = (abinOperator ann) {annTokenPos = AnnSpan opSpan}}
  pure (L span' (NixBinApp ann' op lhs' rhs'))

--------------------------------------------------------------------------------

repairPrefix :: SrcSpan -> AnnPrefixNode -> (AnnPrefixNode -> LExpr -> Expr) -> LExpr -> RepairM LExpr
repairPrefix originalSpan ann mkNode inner = do
  let tokSpan = tokenSpanAt (spanStartCursor originalSpan) (apfxToken ann)
  inner' <- repairChildAfter (expectTokenSpan "prefix operator" (apfxToken ann)) (getLoc inner) tokSpan inner
  let span' = tokSpan `combineSrcSpans` getLoc inner'
      ann' = setAnnSpan span' ann {apfxToken = (apfxToken ann) {annTokenPos = AnnSpan tokSpan}}
  pure (L span' (mkNode ann' inner'))

--------------------------------------------------------------------------------

repairList :: AnnListNode -> [LExpr] -> RepairM LExpr
repairList ann xs = do
  xs' <- mapM repairExpr xs
  repaired <- liftEPResult (rebuildListLayout ann xs')
  pure (L (exprSpan repaired) repaired)

--------------------------------------------------------------------------------

repairSet :: AnnSet -> NixSetIsRecursive -> [LBinding] -> RepairM LExpr
repairSet ann kind bindings = do
  bindings' <- mapM repairBinding bindings
  repaired <- liftEPResult (rebuildSetLayout ann kind bindings')
  pure (L (exprSpan repaired) repaired)

--------------------------------------------------------------------------------

repairLet :: AnnLetNode -> [LBinding] -> LExpr -> RepairM LExpr
repairLet ann bindings body = do
  -- repair bindings
  bindings' <- mapM repairBinding bindings
  -- rebuild let to discover new in
  provisional <- liftEPResult (rebuildLetLayout ann bindings' body)
  let newInSpan = case provisional of
        NixLet ann' _ _ -> expectTokenSpan "in keyword" (alIn ann')
        _ -> error "impossible: rebuildLetLayout did not return let"
      oldInSpan = expectTokenSpan "in keyword" (alIn ann)
      bodyCursor
        | srcSpanStartLine (getLoc body) > srcSpanEndLine oldInSpan = RenderCursor (srcSpanEndLine newInSpan + 1) (srcSpanStartColumn (getLoc body))
        | otherwise = preserveGapTarget oldInSpan (getLoc body) newInSpan
  -- repair body there
  body' <- repairExprAt bodyCursor body
  -- rebuild let again with the repaired body
  repaired <- liftEPResult (rebuildLetLayout ann bindings' body')
  pure (L (exprSpan repaired) repaired)

--------------------------------------------------------------------------------

repairHasAttr :: SrcSpan -> AnnHasAttr -> LExpr -> LAttrPath -> RepairM LExpr
repairHasAttr originalSpan ann lhs path = do
  lhs' <- repairExprAt (spanStartCursor originalSpan) lhs
  let qSpan = preserveGapSpan (getLoc lhs) (expectTokenSpan "has-attr question" (ahaQuestion ann)) lhs'
  path' <- repairAttrPathAt (preserveGapTarget (expectTokenSpan "has-attr question" (ahaQuestion ann)) (getLoc path) qSpan) path
  let span' = getLoc lhs' `combineSrcSpans` attrPathSpan (unLoc path')
      ann' = setAnnSpan span' (prepareHasAttrLayout (ann {ahaQuestion = (ahaQuestion ann) {annTokenPos = AnnSpan qSpan}}) (unLoc lhs') (unLoc path'))
  pure (L span' (NixHasAttr ann' lhs' path'))

--------------------------------------------------------------------------------

repairSelect :: SrcSpan -> AnnSelect -> LExpr -> LAttrPath -> Maybe LExpr -> RepairM LExpr
repairSelect originalSpan ann lhs path def = do
  -- repair lhs from the original root
  lhs' <- repairExprAt (spanStartCursor originalSpan) lhs
  -- move path relative to the repaired lhs
  path' <- repairAttrPathAt (preserveGapTarget (getLoc lhs) (getLoc path) (getLoc lhs')) path
  -- if def exists, repair it relative to the repaired or
  def' <- traverse (repairExprAtSelect ann path path') def
  -- compute new or span from repaired path
  let annWithOr = case (aslOr ann, def, def') of
        (Just orTok, Just _, Just _) ->
          let orSpan = preserveGapSpan (attrPathRenderSpan (unLoc path)) (expectTokenSpan "select or" orTok) path'
           in ann {aslOr = Just (orTok {annTokenPos = AnnSpan orSpan})}
        _ -> ann
      ann' = setAnnSpan finalSpan (prepareSelectLayout annWithOr (unLoc lhs') (unLoc path') def')
      finalSpan = maybe baseSpan (combineSrcSpans baseSpan . getLoc) def'
      baseSpan = getLoc lhs' `combineSrcSpans` attrPathSpan (unLoc path')
  pure (L finalSpan (NixSelect ann' lhs' path' def'))

--------------------------------------------------------------------------------

repairIf :: SrcSpan -> AnnIfNode -> LExpr -> LExpr -> LExpr -> RepairM LExpr
repairIf originalSpan ann cond thenExpr elseExpr = do
  let ifSpan = tokenSpanAt (spanStartCursor originalSpan) (aifIf ann)
  cond' <- repairExprAt (preserveGapTarget (expectTokenSpan "if keyword" (aifIf ann)) (getLoc cond) ifSpan) cond
  let thenSpan = preserveGapSpan (getLoc cond) (expectTokenSpan "then keyword" (aifThen ann)) cond'
  then' <- repairExprAt (preserveGapTarget (expectTokenSpan "then keyword" (aifThen ann)) (getLoc thenExpr) thenSpan) thenExpr
  let elseSpan = preserveGapSpan (getLoc thenExpr) (expectTokenSpan "else keyword" (aifElse ann)) then'
  else' <- repairExprAt (preserveGapTarget (expectTokenSpan "else keyword" (aifElse ann)) (getLoc elseExpr) elseSpan) elseExpr
  let ann0 = ann {aifIf = (aifIf ann) {annTokenPos = AnnSpan ifSpan}, aifThen = (aifThen ann) {annTokenPos = AnnSpan thenSpan}, aifElse = (aifElse ann) {annTokenPos = AnnSpan elseSpan}}
      span' = ifSpan `combineSrcSpans` getLoc else'
      ann' = setAnnSpan span' (prepareIfLayout ann0 (unLoc cond') (unLoc then') (unLoc else'))
  pure (L span' (NixIf ann' cond' then' else'))

--------------------------------------------------------------------------------

repairWith :: SrcSpan -> AnnWithNode -> LExpr -> LExpr -> RepairM LExpr
repairWith originalSpan ann scope body = do
  let withSpan = tokenSpanAt (spanStartCursor originalSpan) (awWith ann)
  scope' <- repairExprAt (preserveGapTarget (expectTokenSpan "with keyword" (awWith ann)) (getLoc scope) withSpan) scope
  let semiSpan = preserveGapSpan (getLoc scope) (expectTokenSpan "with semicolon" (awSemicolon ann)) scope'
  body' <- repairExprAt (preserveGapTarget (expectTokenSpan "with semicolon" (awSemicolon ann)) (getLoc body) semiSpan) body
  let ann0 = ann {awWith = (awWith ann) {annTokenPos = AnnSpan withSpan}, awSemicolon = (awSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
      span' = withSpan `combineSrcSpans` getLoc body'
      ann' = setAnnSpan span' (prepareWithLayout ann0 (unLoc scope') (unLoc body'))
  pure (L span' (NixWith ann' scope' body'))

--------------------------------------------------------------------------------

repairAssert :: SrcSpan -> AnnAssertNode -> LExpr -> LExpr -> RepairM LExpr
repairAssert originalSpan ann assertion body = do
  let assertSpan = tokenSpanAt (spanStartCursor originalSpan) (aaAssert ann)
  assertion' <- repairExprAt (preserveGapTarget (expectTokenSpan "assert keyword" (aaAssert ann)) (getLoc assertion) assertSpan) assertion
  let semiSpan = preserveGapSpan (getLoc assertion) (expectTokenSpan "assert semicolon" (aaSemicolon ann)) assertion'
  body' <- repairExprAt (preserveGapTarget (expectTokenSpan "assert semicolon" (aaSemicolon ann)) (getLoc body) semiSpan) body
  let ann0 = ann {aaAssert = (aaAssert ann) {annTokenPos = AnnSpan assertSpan}, aaSemicolon = (aaSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
      span' = assertSpan `combineSrcSpans` getLoc body'
      ann' = setAnnSpan span' (prepareAssertLayout ann0 (unLoc assertion') (unLoc body'))
  pure (L span' (NixAssert ann' assertion' body'))

--------------------------------------------------------------------------------

repairBinding :: LBinding -> RepairM LBinding
repairBinding (L originalSpan node) =
  case node of
    NixNormalBinding ann path expr -> repairNormalBinding originalSpan ann path expr
    NixInheritBinding ann mScope names -> repairInheritBinding originalSpan ann mScope names

repairNormalBinding :: SrcSpan -> AnnNormalBinding -> LAttrPath -> LExpr -> RepairM LBinding
repairNormalBinding originalSpan ann path expr = do
  path' <- repairAttrPathAt (spanStartCursor originalSpan) path
  let eqSpan = preserveGapSpan (attrPathSpan (unLoc path)) (tokenSpanFromAnchor "binding equals" (attrPathSpan (unLoc path')) (anbEqual ann)) path'
  expr' <- repairExprAt (preserveGapTarget (tokenSpanFromAnchor "binding equals" (attrPathSpan (unLoc path')) (anbEqual ann)) (getLoc expr) eqSpan) expr
  let semiSpan = preserveGapSpan (getLoc expr) (tokenSpanFromAnchor "binding semicolon" (getLoc expr') (anbSemicolon ann)) expr'
      ann' = setAnnSpan span' ann {anbEqual = (anbEqual ann) {annTokenPos = AnnSpan eqSpan}, anbSemicolon = (anbSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
      span' = attrPathSpan (unLoc path') `combineSrcSpans` semiSpan
  pure (L span' (NixNormalBinding ann' path' expr'))

repairInheritBinding :: SrcSpan -> AnnInheritBinding -> Maybe LExpr -> [LAttrKey] -> RepairM LBinding
repairInheritBinding originalSpan ann mScope names = do
  let inheritSpan = tokenSpanAt (spanStartCursor originalSpan) (aibInherit ann)
  scope' <- case mScope of
    Nothing -> pure Nothing
    Just scopeExpr ->
      Just
        <$> repairExprAt
          (preserveGapTarget (expectTokenSpan "inherit keyword" (aibInherit ann)) (getLoc scopeExpr) inheritSpan)
          scopeExpr
  names' <- repairInheritNames inheritSpan mScope scope' names
  let anchorOld = maybe (maybe (expectTokenSpan "inherit keyword" (aibInherit ann)) getLoc (lastMay names)) getLoc mScope
      anchorNew = maybe (maybe inheritSpan getLoc (lastMay names')) getLoc scope'
      semiSpan = preserveGapSpan anchorOld (expectTokenSpan "inherit semicolon" (aibSemicolon ann)) (L anchorNew ())
      span' = foldr combineSrcSpans (inheritSpan `combineSrcSpans` semiSpan) (maybe [] ((: []) . getLoc) scope' <> fmap getLoc names')
      ann' = setAnnSpan span' ann {aibInherit = (aibInherit ann) {annTokenPos = AnnSpan inheritSpan}, aibSemicolon = (aibSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
  pure (L span' (NixInheritBinding ann' scope' names'))

--------------------------------------------------------------------------------

repairAttrPath :: LAttrPath -> RepairM LAttrPath
repairAttrPath path = repairAttrPathAt (spanStartCursor (getLoc path)) path

repairAttrPathAt :: RenderCursor -> LAttrPath -> RepairM LAttrPath
repairAttrPathAt cursor (L _ (NixAttrPath ann keys)) =
  withRepairCursor cursor $
    case keys of
      [] -> liftEPResult (Left EmptyAttrPathEdit)
      firstKey : restKeys -> repairAttrPathHead ann keys firstKey restKeys

repairAttrPathHead :: AnnAttrPath -> [LAttrKey] -> LAttrKey -> [LAttrKey] -> RepairM LAttrPath
repairAttrPathHead ann keys firstKey restKeys = do
  cursor <- currentRepairCursor
  case (leadingDot, dotTokens) of
    (True, firstDot : restDots) -> do
      let firstDotSpan = tokenSpanAt cursor firstDot
      firstKey' <- repairAttrKeyAt (preserveGapTarget (expectTokenSpan "attr path dot" firstDot) (getLoc firstKey) firstDotSpan) firstKey
      (dots', keys') <- repairAttrTail restDots restKeys (getLoc firstKey) firstKey'
      let ann' = setAnnSpan span' ann {aapDots = firstDot {annTokenPos = AnnSpan firstDotSpan} : dots'}
          span' = firstDotSpan `combineSrcSpans` foldr1 combineSrcSpans (getLoc firstKey' : fmap getLoc keys')
      pure (L span' (NixAttrPath ann' (firstKey' : keys')))
    _ -> do
      firstKey' <- repairAttrKeyAt cursor firstKey
      (dots', keys') <- repairAttrTail dotTokens restKeys (getLoc firstKey) firstKey'
      let span' = foldr1 combineSrcSpans (getLoc firstKey' : fmap getLoc keys')
          ann' = setAnnSpan span' ann {aapDots = dots'}
      pure (L span' (NixAttrPath ann' (firstKey' : keys')))
  where
    leadingDot = length (aapDots ann) == length keys && not (null (aapDots ann))
    dotTokens = aapDots ann

--------------------------------------------------------------------------------
repairFuncPat :: LFuncPat -> RepairM LFuncPat
repairFuncPat pat = repairFuncPatAt (spanStartCursor (getLoc pat)) pat

repairFuncPatAt :: RenderCursor -> LFuncPat -> RepairM LFuncPat
repairFuncPatAt cursor (L _ node) =
  withRepairCursor cursor $
    case node of
      NixVarPat ann ident ->
        let ident' = repairLocatedTextAt cursor ident
            span' = getLoc ident'
            ann' = setAnnSpan span' ann {avpId = span'}
         in pure (L span' (NixVarPat ann' ident'))
      NixSetPat ann ellipses mAs bindings -> repairSetPat ann ellipses mAs bindings

repairSetPat :: AnnSetPatNode -> NixSetPatEllipses -> Maybe LSetPatAs -> [LSetPatBinding] -> RepairM LFuncPat
repairSetPat ann ellipses mAs bindings = do
  cursor <- currentRepairCursor
  let openSpan = tokenSpanAt cursor (aspOpenC ann)
  mAs' <- traverse (repairSetPatAsAt cursor) mAs
  let entryCursor = setPatEntryCursor openSpan mAs'
  bindings' <- repairSetPatBindingsAt ann entryCursor bindings
  let closeSpan = repairSetPatCloseSpan ann bindings' ellipses
      ann' = setAnnSpan patSpan ann {aspOpenC = (aspOpenC ann) {annTokenPos = AnnSpan openSpan}, aspCloseC = (aspCloseC ann) {annTokenPos = AnnSpan closeSpan}}
      patNode = NixSetPat ann' ellipses mAs' bindings'
      patSpan = funcPatBodySpan patNode
  pure (L patSpan patNode)

setPatEntryCursor :: SrcSpan -> Maybe LSetPatAs -> RenderCursor
setPatEntryCursor openSpan = \case
  Just asPat@(L _ NixSetPatAs {nspaLocation = NixSetPatAsLeading}) -> spanStartCursor (getLoc asPat)
  _ -> RenderCursor (srcSpanStartLine openSpan) (srcSpanEndColumn openSpan)

--------------------------------------------------------------------------------

repairAttrKeyAt :: RenderCursor -> LAttrKey -> RepairM LAttrKey
repairAttrKeyAt cursor (L _ key) =
  let span' = case key of
        NixStaticAttrKey _ ident -> textSpanAt (cursorFile ident) cursor (unLoc ident)
        NixDynamicStringAttrKey _ _ -> textSpanAt cursorFallbackFile cursor (renderToText key)
        NixDynamicInterpolAttrKey _ _ -> textSpanAt cursorFallbackFile cursor (renderToText key)
      key' = case key of
        NixStaticAttrKey ann ident -> NixStaticAttrKey ann (repairLocatedTextAt cursor ident)
        other -> other
   in pure (L span' key')

--------------------------------------------------------------------------------

repairLocatedLitAt :: RenderCursor -> LLit -> LLit
repairLocatedLitAt cursor (L _ lit) = L (textSpanAt cursorFallbackFile cursor (renderToText lit)) lit

repairLocatedStringAt :: RenderCursor -> LNString -> LNString
repairLocatedStringAt cursor (L _ str) = L (textSpanAt cursorFallbackFile cursor (renderStringText str)) str
  where
    renderStringText = \case
      NixDoubleQuotesString src _ -> renderDoubleQuotedSourceText src
      NixDoubleSingleQuotesString src _ -> renderIndentedStringSourceText src

repairLocatedPathAt :: RenderCursor -> LPath -> LPath
repairLocatedPathAt cursor (L _ path) = L (textSpanAt cursorFallbackFile cursor (renderToText path)) path

repairLocatedTextAt :: RenderCursor -> Located Text -> Located Text
repairLocatedTextAt cursor (L _ txt) = L (textSpanAt cursorFallbackFile cursor txt) txt

--------------------------------------------------------------------------------

repairSetPatAsAt :: RenderCursor -> LSetPatAs -> RepairM LSetPatAs
repairSetPatAsAt cursor (L _ asPat@NixSetPatAs {..}) =
  let var' = repairLocatedTextAt cursor nspaVar
      atSpan = case nspaLocation of
        NixSetPatAsLeading -> tokenSpanAt (spanEndCursor (getLoc var')) (aspaAt nspaAnn)
        NixSetPatAsTrailing -> tokenSpanAt cursor (aspaAt nspaAnn)
      span' = setPatAsRenderSpan (nspaAnn {aspaAt = (aspaAt nspaAnn) {annTokenPos = AnnSpan atSpan}}) var' nspaLocation
      ann' = setAnnSpan span' nspaAnn {aspaAt = (aspaAt nspaAnn) {annTokenPos = AnnSpan atSpan}}
   in pure (L span' asPat {nspaAnn = ann', nspaVar = var'})

repairSetPatBindingsAt :: AnnSetPatNode -> RenderCursor -> [LSetPatBinding] -> RepairM [LSetPatBinding]
repairSetPatBindingsAt ann startCursor bindings = reverse . snd <$> foldM step (startCursor, []) (zip [0 ..] bindings)
  where
    step (!cursor, repaired) (idx, binding) = do
      binding' <- repairSetPatBindingAt ann idx cursor binding
      let next = advanceCursor (spanStartCursor (getLoc binding')) (renderToText (unLoc binding'))
      pure (next, binding' : repaired)

repairSetPatBindingAt :: AnnSetPatNode -> Int -> RenderCursor -> LSetPatBinding -> RepairM LSetPatBinding
repairSetPatBindingAt _ _ cursor (L _ binding@NixSetPatBinding {..}) = do
  let var' = repairLocatedTextAt cursor nspbVar
  def' <- case nspbDefault of
    Nothing -> pure Nothing
    Just defExpr ->
      Just
        <$> repairExprAt
          (preserveGapTargetForSetPat binding var' defExpr)
          defExpr
  let qTok' = case (aspbQuestion nspbAnn, nspbDefault, def') of
        (Just qTok, Just _, Just _) ->
          let qSpan = preserveGapSpan (getLoc nspbVar) (expectTokenSpan "set pattern question" qTok) var'
           in Just (qTok {annTokenPos = AnnSpan qSpan})
        _ -> aspbQuestion nspbAnn
      binding' = NixSetPatBinding (setAnnSpan span' nspbAnn {aspbQuestion = qTok'}) var' def'
      span' = maybe (getLoc var') (combineSrcSpans (getLoc var') . getLoc) def'
  pure (L span' binding')

--------------------------------------------------------------------------------

repairAttrTail :: [AnnToken] -> [LAttrKey] -> SrcSpan -> LAttrKey -> RepairM ([AnnToken], [LAttrKey])
repairAttrTail [] [] _ _ = pure ([], [])
repairAttrTail (dotTok : dotToks) (key : keys) oldPrev newPrev = do
  let dotSpan = preserveGapSpan oldPrev (expectTokenSpan "attr path dot" dotTok) (L (getLoc newPrev) ())
  key' <- repairAttrKeyAt (preserveGapTarget dotSpan (getLoc key) dotSpan) key
  (dots', keys') <- repairAttrTail dotToks keys (getLoc key) key'
  pure (dotTok {annTokenPos = AnnSpan dotSpan} : dots', key' : keys')
repairAttrTail dots keys _ _ = liftEPResult (Left (IndexOutOfRange (length dots) (length keys)))

--------------------------------------------------------------------------------

repairInheritNames :: SrcSpan -> Maybe LExpr -> Maybe LExpr -> [LAttrKey] -> RepairM [LAttrKey]
repairInheritNames inheritSpan oldScope newScope names = third <$> foldM step (oldStartAnchor, startAnchor, []) names
  where
    third (_, _, repaired) = repaired
    startAnchor = maybe inheritSpan getLoc newScope
    oldStartAnchor = maybe inheritSpan getLoc oldScope
    step (prevOld, prevNew, repaired) name = do
      name' <- repairAttrKeyAt (preserveGapTarget prevOld (getLoc name) prevNew) name
      pure (getLoc name, getLoc name', repaired <> [name'])

--------------------------------------------------------------------------------

tokenSpanFromAnchor :: Text -> SrcSpan -> AnnToken -> SrcSpan
tokenSpanFromAnchor label anchor tok =
  case annTokenSrcSpan tok of
    Just span' -> span'
    Nothing -> case annTokenDelta tok of
      Just delta -> applyDeltaToAnchor anchor delta
      Nothing -> error (T.unpack label <> ": missing token span")

--------------------------------------------------------------------------------

repairExprAtSelect :: AnnSelect -> LAttrPath -> LAttrPath -> LExpr -> RepairM LExpr
repairExprAtSelect ann oldPath newPath oldDef =
  case aslOr ann of
    Just orTok ->
      let orSpan = preserveGapSpan (attrPathRenderSpan (unLoc oldPath)) (expectTokenSpan "select or" orTok) newPath
       in repairExprAt (preserveGapTarget (expectTokenSpan "select or" orTok) (getLoc oldDef) orSpan) oldDef
    Nothing -> repairExpr oldDef

--------------------------------------------------------------------------------

liftEPResult :: EPResult a -> RepairM a
liftEPResult = ReaderT . const

--------------------------------------------------------------------------------

repairSetPatCloseSpan :: AnnSetPatNode -> [LSetPatBinding] -> NixSetPatEllipses -> SrcSpan
repairSetPatCloseSpan ann bindings _ =
  case reverse bindings of
    lastBinding : _ ->
      let endCursor = advanceCursor (spanStartCursor (getLoc lastBinding)) (renderToText (unLoc lastBinding))
       in tokenSpanAt endCursor (aspCloseC ann)
    [] -> expectTokenSpan "set pattern close" (aspCloseC ann)
