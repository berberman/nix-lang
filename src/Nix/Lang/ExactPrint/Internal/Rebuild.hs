-- | Container rebuilding and layout normalization for internal exact-print preparation.
--
-- This module owns the logic that reflows binding and element sequences while
-- preserving exact-print token/comment relationships.
module Nix.Lang.ExactPrint.Internal.Rebuild
  ( BindingSequenceAnchor (..),
    rebuildSetLayout,
    rebuildSetLayoutWithAnchor,
    rebuildLetLayout,
    rebuildLetLayoutWithAnchor,
    rebuildListLayout,
    rebuildListLayoutWithAnchor,
    normalizeBindingLayout,
    normalizeAttrPathLayout,
    normalizeExprLayout,
  )
where

import Data.Data (Data)
import Data.Text (Text)
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint.Internal.Geometry
import Nix.Lang.ExactPrint.Internal.Types
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.ExactPrint.Internal.Reflow (Flow (..), closeAfter, reflow)
import Nix.Lang.Outputable (output)
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Types.Parsed
import Nix.Lang.Utils
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

-- | Layout style inferred for set-like binding containers.
data SetLayoutStyle = SetInline | SetMultiline

-- | Layout style inferred for list containers.
data ListLayoutStyle = ListInline | ListMultiline

-- | Layout style inferred for the body following an @in@ token.
data BodyLayoutStyle = BodyInline | BodyMultiline

-- | How rebuild should choose the first anchor for a sequence.
data BindingSequenceAnchor = AnchorPreserveExisting | AnchorStartAtFirstSlot

-- | Rebuild a set from its current bindings while preserving exact-print metadata.
rebuildSetLayout :: AnnSet -> NixSetIsRecursive -> [LBinding] -> ExactPrintResult Expr
rebuildSetLayout ann kind bindings = rebuildSetLayoutWithAnchor AnchorPreserveExisting ann kind bindings

-- | Rebuild a set using an explicit anchor policy.
--
-- Algorithm:
--
-- * infer inline vs multiline style from the existing annotated shape,
-- * choose the first binding cursor from that style and anchor policy,
-- * reflow bindings left-to-right,
-- * place the close token after the repaired binding sequence,
-- * shift trailing comments with the close token,
-- * normalize the final annotation for exact printing.
rebuildSetLayoutWithAnchor :: BindingSequenceAnchor -> AnnSet -> NixSetIsRecursive -> [LBinding] -> ExactPrintResult Expr
rebuildSetLayoutWithAnchor anchorMode ann kind bindings =
  Right $ NixSet ann' kind (L bindingsSpan shiftedBindings)
  where
    openSpan = expectTokenSpan "set open brace" (asOpenC ann)
    closeSpan = expectTokenSpan "set close brace" (asCloseC ann)
    targetFile = srcSpanFilename openSpan
    style = inferSetLayoutStyle ann bindings
    firstCursor = firstBindingCursor anchorMode style ann bindings
    shiftedBindings = rebuildBindingsLayout targetFile style firstCursor bindings
    closeStart = closeCursor style closeSpan shiftedBindings
    newCloseSpan = tokenSpanAt closeStart (asCloseC ann)
    closeShiftedComments = shiftComments closeSpan newCloseSpan (followingComments (annComments ann))
    annWithComments = setAnnCommon commonWithComments ann
    annWithClose = annWithComments {asCloseC = closeToken}
    preparedAnn = prepareSetLayout annWithClose shiftedBindings
    ann' = preparedAnn {asCloseC = closeToken}
    closeToken = (asCloseC ann) {annTokenPos = AnnSpan newCloseSpan}
    commonWithComments =
      (getAnnCommon ann)
        { acComments =
            (annComments ann)
              { followingComments = closeShiftedComments
              },
          acPos = AnnSpan setSpan
        }
    setSpan =
      foldr combineSrcSpans base (bindingSpan . unLoc <$> shiftedBindings)
    base = maybe openSpan (`combineSrcSpans` openSpan) (annTokenSrcSpan =<< asRec ann) `combineSrcSpans` newCloseSpan
    bindingsSpan = listSpanOr base shiftedBindings

-- | Rebuild a @let ... in ...@ container from its current children.
rebuildLetLayout :: AnnLetNode -> [LBinding] -> LExpr -> ExactPrintResult Expr
rebuildLetLayout ann bindings body =
  rebuildLetLayoutWithAnchor AnchorPreserveExisting ann bindings body

-- | Rebuild a @let ... in ...@ container using an explicit anchor policy.
--
-- This follows the same sequence algorithm as sets, but also computes a fresh
-- @in@ token position and then reanchors the body relative to that token.
rebuildLetLayoutWithAnchor :: BindingSequenceAnchor -> AnnLetNode -> [LBinding] -> LExpr -> ExactPrintResult Expr
rebuildLetLayoutWithAnchor anchorMode ann bindings body =
  Right $ NixLet finalAnn (L bindingsSpan shiftedBindings) shiftedBody
  where
    letSpan = expectTokenSpan "let keyword" (alLet ann)
    oldInSpan = expectTokenSpan "in keyword" (alIn ann)
    targetFile = srcSpanFilename letSpan
    bindingStyle = inferLetBindingLayoutStyle ann bindings
    bodyStyle = inferBodyLayoutStyle oldInSpan (getLoc body)
    firstCursor = firstLetBindingCursor anchorMode bindingStyle ann bindings
    shiftedBindings = rebuildBindingsLayout targetFile bindingStyle firstCursor bindings
    inStart = letInCursor bindingStyle oldInSpan shiftedBindings
    newInSpan = tokenSpanAt inStart (alIn ann)
    shiftedBody = rebuildLetBodyLayout bodyStyle oldInSpan newInSpan body
    inToken = (alIn ann) {annTokenPos = AnnSpan newInSpan}
    annWithIn = ann {alIn = inToken}
    preparedAnn = prepareLetLayout annWithIn shiftedBindings (unLoc shiftedBody)
    base = letSpan `combineSrcSpans` newInSpan `combineSrcSpans` getLoc shiftedBody
    letSpan' = foldr combineSrcSpans base (bindingSpan . unLoc <$> shiftedBindings)
    annCommon' = (getAnnCommon preparedAnn) {acPos = AnnSpan letSpan'}
    finalAnn = setAnnCommon annCommon' (preparedAnn {alIn = inToken})
    bindingsSpan = listSpanOr (letSpan `combineSrcSpans` newInSpan) shiftedBindings

-- | Rebuild a list expression from its current elements.
rebuildListLayout :: AnnListNode -> [LExpr] -> ExactPrintResult Expr
rebuildListLayout ann xs =
  rebuildListLayoutWithAnchor AnchorPreserveExisting ann xs

-- | Rebuild a list expression using an explicit anchor policy.
--
-- This is the list analogue of 'rebuildSetLayoutWithAnchor': it reflows the
-- element sequence, then recomputes the closing bracket and any trailing
-- comments attached to it.
rebuildListLayoutWithAnchor :: BindingSequenceAnchor -> AnnListNode -> [LExpr] -> ExactPrintResult Expr
rebuildListLayoutWithAnchor anchorMode ann xs =
  Right $ NixList ann' shiftedElems
  where
    openSpan = expectTokenSpan "list open bracket" (alnOpenS ann)
    closeSpan = expectTokenSpan "list close bracket" (alnCloseS ann)
    targetFile = srcSpanFilename openSpan
    style = inferListLayoutStyle ann xs
    firstCursor = firstListElementCursor anchorMode style ann xs
    shiftedElems = rebuildListElements targetFile style firstCursor xs
    closeStart = listCloseCursor style closeSpan shiftedElems
    newCloseSpan = tokenSpanAt closeStart (alnCloseS ann)
    closeShiftedComments = shiftComments closeSpan newCloseSpan (followingComments (annComments ann))
    annWithComments = setAnnCommon commonWithComments ann
    annWithClose = annWithComments {alnCloseS = closeToken}
    ann' = prepareListLayout annWithClose shiftedElems
    closeToken = (alnCloseS ann) {annTokenPos = AnnSpan newCloseSpan}
    base = openSpan `combineSrcSpans` newCloseSpan
    listSpan' = foldr combineSrcSpans base (exprSpan . unLoc <$> shiftedElems)
    commonWithComments =
      (getAnnCommon ann)
        { acComments =
            (annComments ann)
              { followingComments = closeShiftedComments
              },
          acPos = AnnSpan listSpan'
        }

-- | Infer whether a @let@ binding list should stay inline or multiline.
inferLetBindingLayoutStyle :: AnnLetNode -> [LBinding] -> SetLayoutStyle
inferLetBindingLayoutStyle ann bindings =
  case bindings of
    firstBinding : _
      | srcSpanStartLine (getLoc firstBinding) == srcSpanStartLine letSpan
          && srcSpanStartLine inSpan == srcSpanStartLine letSpan ->
          SetInline
    []
      | srcSpanStartLine inSpan == srcSpanStartLine letSpan -> SetInline
    _ -> SetMultiline
  where
    letSpan = expectTokenSpan "let keyword" (alLet ann)
    inSpan = expectTokenSpan "in keyword" (alIn ann)

-- | Choose the first binding cursor for a repaired @let@.
--
-- When preserving existing anchors, this prefers the old first binding span if
-- it is still compatible with the inferred layout style.
firstLetBindingCursor :: BindingSequenceAnchor -> SetLayoutStyle -> AnnLetNode -> [LBinding] -> RenderCursor
firstLetBindingCursor anchorMode style ann bindings =
  case (style, bindings) of
    (SetInline, firstBinding : _)
      | preserveExisting
          && srcSpanStartLine (getLoc firstBinding) == srcSpanEndLine letSpan
          && srcSpanStartColumn (getLoc firstBinding) > srcSpanEndColumn letSpan ->
          cursorAtSpanStart (getLoc firstBinding)
      | otherwise -> inlineInsertCursor letSpan inSpan
    (SetInline, []) -> inlineInsertCursor letSpan inSpan
    (SetMultiline, firstBinding : _)
      | preserveExisting -> cursorAtSpanStart (getLoc firstBinding)
      | otherwise -> RenderCursor (srcSpanStartLine letSpan + 1) (srcSpanStartColumn letSpan + 2)
    (SetMultiline, []) -> RenderCursor (srcSpanStartLine letSpan + 1) (srcSpanStartColumn letSpan + 2)
  where
    letSpan = expectTokenSpan "let keyword" (alLet ann)
    inSpan = expectTokenSpan "in keyword" (alIn ann)
    preserveExisting = case anchorMode of
      AnchorPreserveExisting -> True
      AnchorStartAtFirstSlot -> False

-- | Compute where the @in@ token should be placed after repairing bindings.
letInCursor :: SetLayoutStyle -> SrcSpan -> [LBinding] -> RenderCursor
letInCursor style oldIn bindings =
  case (style, reverse bindings) of
    (_, []) -> cursorAtSpanStart oldIn
    (SetInline, lastBinding : _) ->
      advanceCursor (cursorAtSpanStart (getLoc lastBinding)) (renderBindingSyntax (unLoc lastBinding) <> " ")
    (SetMultiline, lastBinding : _) ->
      let endCursor = advanceCursor (cursorAtSpanStart (getLoc lastBinding)) (renderBindingSyntax (unLoc lastBinding))
       in RenderCursor (rcLine endCursor + 1) (srcSpanStartColumn oldIn)

-- | Infer whether the body after @in@ should stay inline or move to a new line.
inferBodyLayoutStyle :: SrcSpan -> SrcSpan -> BodyLayoutStyle
inferBodyLayoutStyle anchor bodySpan
  | srcSpanStartLine bodySpan == srcSpanEndLine anchor = BodyInline
  | otherwise = BodyMultiline

-- | Reanchor the body of a @let@ relative to the repaired @in@ token.
rebuildLetBodyLayout :: BodyLayoutStyle -> SrcSpan -> SrcSpan -> LExpr -> LExpr
rebuildLetBodyLayout style oldInSpan newInSpan body =
  normalizeExprLayout (translateFromTo (getLoc body) targetSpan body)
  where
    targetSpan = case style of
      BodyInline ->
        mkSrcSpan
          (srcSpanFilename newInSpan)
          (srcSpanEndLine newInSpan, srcSpanEndColumn newInSpan + 1)
          (srcSpanEndLine newInSpan, srcSpanEndColumn newInSpan + 2)
      BodyMultiline ->
        let bodyDelta = deltaFromAnchor oldInSpan (getLoc body)
         in applyDeltaToAnchor newInSpan bodyDelta

-- | Infer whether a list should stay inline or multiline.
inferListLayoutStyle :: AnnListNode -> [LExpr] -> ListLayoutStyle
inferListLayoutStyle ann xs =
  case xs of
    firstElem : _
      | srcSpanStartLine (getLoc firstElem) == srcSpanStartLine openSpan
          && srcSpanStartLine closeSpan == srcSpanStartLine openSpan ->
          ListInline
    []
      | srcSpanStartLine closeSpan == srcSpanStartLine openSpan -> ListInline
    _ -> ListMultiline
  where
    openSpan = expectTokenSpan "list open bracket" (alnOpenS ann)
    closeSpan = expectTokenSpan "list close bracket" (alnCloseS ann)

-- | Choose the first element cursor for a repaired list.
firstListElementCursor :: BindingSequenceAnchor -> ListLayoutStyle -> AnnListNode -> [LExpr] -> RenderCursor
firstListElementCursor anchorMode style ann xs =
  case (style, xs) of
    (ListInline, firstElem : _)
      | preserveExisting
          && srcSpanStartLine (getLoc firstElem) == srcSpanEndLine openSpan
          && srcSpanStartColumn (getLoc firstElem) > srcSpanEndColumn openSpan ->
          cursorAtSpanStart (getLoc firstElem)
      | otherwise -> inlineInsertCursor openSpan closeSpan
    (ListInline, []) -> inlineInsertCursor openSpan closeSpan
    (ListMultiline, firstElem : _)
      | preserveExisting -> cursorAtSpanStart (getLoc firstElem)
      | otherwise -> RenderCursor (srcSpanStartLine openSpan + 1) (srcSpanStartColumn openSpan + 2)
    (ListMultiline, []) -> RenderCursor (srcSpanStartLine openSpan + 1) (srcSpanStartColumn openSpan + 2)
  where
    openSpan = expectTokenSpan "list open bracket" (alnOpenS ann)
    closeSpan = expectTokenSpan "list close bracket" (alnCloseS ann)
    preserveExisting = preservesExistingAnchors anchorMode

-- | Reflow list elements from left to right.
rebuildListElements :: String -> ListLayoutStyle -> RenderCursor -> [LExpr] -> [LExpr]
rebuildListElements targetFile style startCursor =
  rebuildSequenceLayout (listFlow style) renderExprSyntax reanchorExpr normalizeExprLayout targetFile startCursor

-- | Compute where the closing bracket should be placed after repairing a list.
listCloseCursor :: ListLayoutStyle -> SrcSpan -> [LExpr] -> RenderCursor
listCloseCursor style oldClose xs =
  closeSequenceCursor (listFlow style) renderExprSyntax oldClose xs

-- | Move an expression so its outer span starts at a given cursor.
reanchorExpr :: String -> RenderCursor -> LExpr -> LExpr
reanchorExpr = reanchorLocated

-- | Infer whether a set should stay inline or multiline.
inferSetLayoutStyle :: AnnSet -> [LBinding] -> SetLayoutStyle
inferSetLayoutStyle ann bindings =
  case bindings of
    firstBinding : _
      | srcSpanStartLine (getLoc firstBinding) == srcSpanStartLine openSpan
          && srcSpanStartLine closeSpan == srcSpanStartLine openSpan ->
          SetInline
    []
      | srcSpanStartLine closeSpan == srcSpanStartLine openSpan -> SetInline
    _ -> SetMultiline
  where
    openSpan = expectTokenSpan "set open brace" (asOpenC ann)
    closeSpan = expectTokenSpan "set close brace" (asCloseC ann)

-- | Choose the first binding cursor for a repaired set.
firstBindingCursor :: BindingSequenceAnchor -> SetLayoutStyle -> AnnSet -> [LBinding] -> RenderCursor
firstBindingCursor anchorMode style ann bindings =
  case (style, bindings) of
    (SetInline, firstBinding : _)
      | preserveExisting
          && srcSpanStartLine (getLoc firstBinding) == srcSpanEndLine openSpan
          && srcSpanStartColumn (getLoc firstBinding) > srcSpanEndColumn openSpan ->
          cursorAtSpanStart (getLoc firstBinding)
      | otherwise -> inlineInsertCursor openSpan closeSpan
    (SetInline, []) ->
      inlineInsertCursor openSpan closeSpan
    (SetMultiline, firstBinding : _)
      | preserveExisting -> cursorAtSpanStart (getLoc firstBinding)
      | otherwise -> RenderCursor (srcSpanStartLine openSpan + 1) (srcSpanStartColumn openSpan + 2)
    (SetMultiline, []) ->
      RenderCursor (srcSpanStartLine openSpan + 1) (srcSpanStartColumn openSpan + 2)
  where
    openSpan = expectTokenSpan "set open brace" (asOpenC ann)
    closeSpan = expectTokenSpan "set close brace" (asCloseC ann)
    preserveExisting = preservesExistingAnchors anchorMode

-- | Choose the inline insertion cursor between an opening and closing token.
inlineInsertCursor :: SrcSpan -> SrcSpan -> RenderCursor
inlineInsertCursor openSpan closeSpan =
  let inlineGap = max 1 (srcSpanStartColumn closeSpan - srcSpanEndColumn openSpan)
   in RenderCursor (srcSpanEndLine openSpan) (srcSpanEndColumn openSpan + inlineGap)

-- | Whether a rebuild should reuse old anchors where possible.
preservesExistingAnchors :: BindingSequenceAnchor -> Bool
preservesExistingAnchors = \case
  AnchorPreserveExisting -> True
  AnchorStartAtFirstSlot -> False

-- | Reflow bindings from left to right.
rebuildBindingsLayout :: String -> SetLayoutStyle -> RenderCursor -> [LBinding] -> [LBinding]
rebuildBindingsLayout targetFile style startCursor =
  rebuildSequenceLayout (setFlow style) renderBindingSyntax reanchorBinding normalizeBindingLayout targetFile startCursor

-- | Compute where the closing brace should be placed after repairing bindings.
closeCursor :: SetLayoutStyle -> SrcSpan -> [LBinding] -> RenderCursor
closeCursor style oldClose bindings =
  closeSequenceCursor (setFlow style) renderBindingSyntax oldClose bindings

-- | Move a binding so its outer span starts at a given cursor.
reanchorBinding :: String -> RenderCursor -> LBinding -> LBinding
reanchorBinding = reanchorLocated

reanchorLocated :: (Data a) => String -> RenderCursor -> Located a -> Located a
reanchorLocated targetFile target located@(L originalSpan _) =
  translateFromTo originalSpan targetSpan located
  where
    targetSpan = mkSrcSpan targetFile (rcLine target, rcColumn target) (rcLine target, rcColumn target + 1)

-- | Convert set layout style into the generic left-to-right flow mode.
setFlow :: SetLayoutStyle -> Flow
setFlow = \case
  SetInline -> FlowInline
  SetMultiline -> FlowMultiline

-- | Convert list layout style into the generic left-to-right flow mode.
listFlow :: ListLayoutStyle -> Flow
listFlow = \case
  ListInline -> FlowInline
  ListMultiline -> FlowMultiline

-- | Render a binding for cursor advancement during rebuild.
renderBindingSyntax :: Binding -> Text
renderBindingSyntax = renderStrict . layoutPretty defaultLayoutOptions . output

-- | Render an expression for cursor advancement during rebuild.
renderExprSyntax :: Expr -> Text
renderExprSyntax = renderStrict . layoutPretty defaultLayoutOptions . output

rebuildSequenceLayout :: Flow -> (a -> Text) -> (String -> RenderCursor -> Located a -> Located a) -> (Located a -> Located a) -> String -> RenderCursor -> [Located a] -> [Located a]
rebuildSequenceLayout flow renderSyntax reanchor normalize targetFile startCursor =
  reflow flow renderSyntax (\cursor -> normalize . reanchor targetFile cursor) startCursor

closeSequenceCursor :: Flow -> (a -> Text) -> SrcSpan -> [Located a] -> RenderCursor
closeSequenceCursor = closeAfter

-- | Normalize a binding subtree after it has been structurally moved.
normalizeBindingLayout :: LBinding -> LBinding
normalizeBindingLayout (L l binding) = L l $ case binding of
  NixNormalBinding ann path expr ->
    let path' = normalizeAttrPathLayout path
        equalTok = case annTokenSrcSpan (anbEqual ann) of
          Just eqSpan -> mapTokenToDelta (deltaFromAnchor (attrPathSpan (unLoc path')) eqSpan) (anbEqual ann)
          Nothing -> anbEqual ann
        expr' = case annTokenSrcSpan (anbEqual ann) of
          Just eqSpan ->
            let targetExprSpan = mkSrcSpan (srcSpanFilename eqSpan) (srcSpanEndLine eqSpan, srcSpanEndColumn eqSpan + 1) (srcSpanEndLine eqSpan, srcSpanEndColumn eqSpan + 2)
             in normalizeExprLayout (translateFromTo (getLoc expr) targetExprSpan expr)
          Nothing -> normalizeExprLayout expr
        semTok = case annTokenSrcSpan (anbSemicolon ann) of
          Just _ -> mapTokenToDelta (DeltaPos 0 0) (anbSemicolon ann)
          Nothing -> anbSemicolon ann
     in NixNormalBinding ann {anbEqual = equalTok, anbSemicolon = semTok} path' expr'
  other -> other

-- | Normalize an attribute-path subtree after it has been structurally moved.
normalizeAttrPathLayout :: LAttrPath -> LAttrPath
normalizeAttrPathLayout (L l (NixAttrPath ann keys)) = L l $ NixAttrPath ann' keys
  where
    ann' = ann {aapDots = normalizeDots keys (aapDots ann)}
    normalizeDots pathKeys dots = zipWith mkDot pathKeys (take (max 0 (length pathKeys - 1)) (dots <> repeat fallbackDot))
    mkDot key tok = case annTokenSrcSpan tok of
      Just dotSpan -> mapTokenToDelta (deltaFromAnchor (getLoc key) dotSpan) tok
      Nothing -> deltaAnnToken AnnDot (DeltaPos 0 0)
    fallbackDot = deltaAnnToken AnnDot (DeltaPos 0 0)

-- | Normalize an expression subtree after it has been structurally moved.
--
-- This rewrites layout-sensitive token annotations relative to the repaired
-- child spans while preserving the existing AST shape.
normalizeExprLayout :: LExpr -> LExpr
normalizeExprLayout (L l expr) = L l $ case expr of
  NixPar ann inner -> NixPar (prepareParLayout ann (unLoc inner')) inner'
    where
      inner' = normalizeExprLayout inner
  NixList ann xs -> NixList (prepareListLayout ann xs') xs'
    where
      xs' = fmap normalizeExprLayout xs
  NixSet ann kind (L bindingsLoc bindings) ->
    let bindings' = fmap normalizeBindingLayout bindings
        ann' = prepareSetLayout ann bindings'
     in NixSet ann' kind (L bindingsLoc bindings')
  NixLet ann (L bindingsLoc bindings) body ->
    let bindings' = fmap normalizeBindingLayout bindings
        body' = normalizeExprLayout body
        ann' = prepareLetLayout ann bindings' (unLoc body')
     in NixLet ann' (L bindingsLoc bindings') body'
  NixHasAttr ann lhs path ->
    let lhs' = normalizeExprLayout lhs
        path' = normalizeAttrPathLayout path
     in NixHasAttr (prepareHasAttrLayout ann (unLoc lhs') (unLoc path')) lhs' path'
  NixSelect ann lhs path def ->
    let lhs' = normalizeExprLayout lhs
        path' = normalizeAttrPathLayout path
        def' = fmap normalizeExprLayout def
     in NixSelect (prepareSelectLayout ann (unLoc lhs') (unLoc path') def') lhs' path' def'
  NixIf ann cond thenExpr elseExpr ->
    let cond' = normalizeExprLayout cond
        then' = normalizeExprLayout thenExpr
        else' = normalizeExprLayout elseExpr
     in NixIf (prepareIfLayout ann (unLoc cond') (unLoc then') (unLoc else')) cond' then' else'
  NixWith ann scope body ->
    let scope' = normalizeExprLayout scope
        body' = normalizeExprLayout body
     in NixWith (prepareWithLayout ann (unLoc scope') (unLoc body')) scope' body'
  NixAssert ann assertion body ->
    let assertion' = normalizeExprLayout assertion
        body' = normalizeExprLayout body
     in NixAssert (prepareAssertLayout ann (unLoc assertion') (unLoc body')) assertion' body'
  other -> other
