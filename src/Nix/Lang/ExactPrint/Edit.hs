-- | Low-level exact-print editing and repair operations.
--
-- The exported @repair*Layout@ functions are the public APIs for
-- repairing AST nodes after structural mutation so they can be exact-printed
-- again.
--
-- Container-specific layout mechanics live in 'Nix.Lang.ExactPrint.Reflow';
-- this module focuses on edit entry points, syntax-aware rebuilding, and
-- recursive repair.
--
-- The implementation is organized in layers:
--
-- * typed fragment parsers and typed edit primitives over sets, lets, and lists
-- * text convenience wrappers built on those typed operations
-- * public structural repair entry points
-- * container-specific rebuild primitives
-- * recursive repair walkers for expressions, bindings, attribute paths, and function patterns
-- * low-level cursor/span utilities used by repair logic
module Nix.Lang.ExactPrint.Edit
  ( EditError (..),
    BindingInsertPosition (..),
    ListInsertPosition (..),
    EditResult,
    parseExprFragment,
    parseBindingFragment,
    parseAttrKeyFragment,
    bindingSpan,
    bindingRenderSpan,
    bindingComments,
    insertSetBindingNodeAt,
    replaceSetBindingNodeAt,
    insertBindingNodeAt,
    replaceBindingNodeAt,
    replaceBindingValueNodeAt,
    insertListElementNodeAt,
    replaceListElementNodeAt,
    repairExprLayout,
    repairBindingLayout,
    repairAttrPathLayout,
    repairFuncPatLayout,
    insertBindingAt,
    deleteBindingAt,
    replaceBindingValue,
    insertListElementAt,
    deleteListElementAt,
    renameAttrPathKey,
    repairSetLayout,
    repairLetLayout,
    repairListLayout,
    insertSetBindingText,
    replaceSetBindingText,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT (..), ask, local, runReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint.Edit.Fragment
import Nix.Lang.ExactPrint.Edit.Geometry
import Nix.Lang.ExactPrint.Edit.Rebuild
import Nix.Lang.ExactPrint.Edit.Types
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Outputable (output)
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------

newtype RepairContext = RepairContext
  { repairCursor :: RenderCursor
  }

type RepairM = ReaderT RepairContext EditResult

runRepairAt :: RenderCursor -> RepairM a -> EditResult a
runRepairAt cursor = flip runReaderT (RepairContext cursor)

withRepairCursor :: RenderCursor -> RepairM a -> RepairM a
withRepairCursor cursor = local (\ctx -> ctx {repairCursor = cursor})

currentRepairCursor :: RepairM RenderCursor
currentRepairCursor = repairCursor <$> ask

--------------------------------------------------------------------------------

-- Public edit operations

-- | Insert a parsed binding node into a set expression.
insertSetBindingNodeAt :: BindingInsertPosition -> LBinding -> Expr -> EditResult Expr
insertSetBindingNodeAt position binding = \case
  NixSet ann kind (L _ bindings) -> insertBindingIntoSet position ann kind bindings binding
  _ -> Left NotASet

-- | Replace a set binding with a parsed binding node.
replaceSetBindingNodeAt :: Int -> LBinding -> Expr -> EditResult Expr
replaceSetBindingNodeAt idx replacement = \case
  NixSet ann kind (L _ bindings) -> replaceBindingInSet idx ann kind bindings replacement
  _ -> Left NotASet

-- | Insert a parsed binding node into a set or let binding container.
insertBindingNodeAt :: BindingInsertPosition -> LBinding -> Expr -> EditResult Expr
insertBindingNodeAt position binding expr =
  case expr of
    NixSet ann kind (L _ bindings) -> insertBindingIntoSet position ann kind bindings binding
    NixLet ann (L _ bindings) body -> insertBindingIntoLet position ann bindings body binding
    _ -> Left NotABindingContainer

-- | Replace a binding with a parsed binding node in a set or let container.
replaceBindingNodeAt :: Int -> LBinding -> Expr -> EditResult Expr
replaceBindingNodeAt idx replacement expr =
  case expr of
    NixSet ann kind (L _ bindings) -> replaceBindingInSet idx ann kind bindings replacement
    NixLet ann (L _ bindings) body -> do
      target <- elementAt idx bindings
      let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
      rebuildLetLayout ann (replaceAt idx replacement' bindings) body
    _ -> Left NotABindingContainer

-- | Replace the RHS of a normal binding with a parsed expression node.
replaceBindingValueNodeAt :: Int -> LExpr -> Expr -> EditResult Expr
replaceBindingValueNodeAt idx replacementExpr expr =
  case expr of
    NixSet ann kind (L _ bindings) -> do
      bindings' <- replaceBindingValueInBindings idx replacementExpr bindings
      rebuildSetLayout ann kind bindings'
    NixLet ann (L _ bindings) body -> do
      bindings' <- replaceBindingValueInBindings idx replacementExpr bindings
      rebuildLetLayout ann bindings' body
    _ -> Left NotABindingContainer

-- | Insert a parsed expression node into a list expression.
insertListElementNodeAt :: ListInsertPosition -> LExpr -> Expr -> EditResult Expr
insertListElementNodeAt position element expr =
  case expr of
    NixList ann xs -> do
      idx <- normalizeListInsertIndex position (length xs)
      rebuildListLayout ann (insertAt idx element xs)
    _ -> Left NotAList

-- | Replace a list element with a parsed expression node.
replaceListElementNodeAt :: Int -> LExpr -> Expr -> EditResult Expr
replaceListElementNodeAt idx replacement expr =
  case expr of
    NixList ann xs -> do
      target <- elementAt idx xs
      let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
      rebuildListLayout ann (replaceAt idx replacement' xs)
    _ -> Left NotAList

insertSetBindingText :: BindingInsertPosition -> Text -> Expr -> EditResult Expr
insertSetBindingText position bindingText expr = do
  newBinding <- parseBindingFragment bindingText
  insertSetBindingNodeAt position newBinding expr

-- | Insert a binding parsed from source text into a set or @let@ container.
insertBindingAt :: BindingInsertPosition -> Text -> Expr -> EditResult Expr
insertBindingAt position bindingText expr = do
  binding <- parseBindingFragment bindingText
  insertBindingNodeAt position binding expr

-- | Delete a binding from a set or @let@ container by index.
deleteBindingAt :: Int -> Expr -> EditResult Expr
deleteBindingAt idx expr
  | idx < 0 = Left (NegativeIndex idx)
  | otherwise = case expr of
      NixSet ann kind (L _ bindings) -> do
        bindings' <- deleteAt idx bindings
        rebuildSetLayoutWithAnchor AnchorStartAtFirstSlot ann kind bindings'
      NixLet ann (L _ bindings) body -> do
        bindings' <- deleteAt idx bindings
        rebuildLetLayoutWithAnchor AnchorStartAtFirstSlot ann bindings' body
      _ -> Left NotABindingContainer

replaceSetBindingText :: Int -> Text -> Expr -> EditResult Expr
replaceSetBindingText idx bindingText expr = do
  replacement <- parseBindingFragment bindingText
  replaceSetBindingNodeAt idx replacement expr

-- | Replace the RHS of a normal binding using source text for the new value.
replaceBindingValue :: Int -> Text -> Expr -> EditResult Expr
replaceBindingValue idx valueText expr = do
  replacementExpr <- parseExprFragment valueText
  replaceBindingValueNodeAt idx replacementExpr expr

-- | Insert a list element parsed from source text.
insertListElementAt :: ListInsertPosition -> Text -> Expr -> EditResult Expr
insertListElementAt position elemText expr = do
  element <- parseExprFragment elemText
  insertListElementNodeAt position element expr

-- | Delete a list element by index.
deleteListElementAt :: Int -> Expr -> EditResult Expr
deleteListElementAt idx expr
  | idx < 0 = Left (NegativeIndex idx)
  | otherwise = case expr of
      NixList ann xs -> do
        xs' <- deleteAt idx xs
        rebuildListLayoutWithAnchor AnchorStartAtFirstSlot ann xs'
      _ -> Left NotAList

renameAttrPathKey :: Int -> Int -> Text -> Expr -> EditResult Expr
renameAttrPathKey bindingIdx keyIdx keyText expr = do
  replacementKey <- parseAttrKeyFragment keyText
  case expr of
    NixSet ann kind (L _ bindings) -> do
      bindings' <- renameBindingAttrPathKey bindingIdx keyIdx replacementKey bindings
      rebuildSetLayout ann kind bindings'
    NixLet ann (L _ bindings) body -> do
      bindings' <- renameBindingAttrPathKey bindingIdx keyIdx replacementKey bindings
      rebuildLetLayout ann bindings' body
    _ -> Left NotABindingContainer

--------------------------------------------------------------------------------

-- Public repair operations

-- | Rebuild the layout of a set expression from its current bindings.
repairSetLayout :: Expr -> EditResult Expr
repairSetLayout = \case
  NixSet ann kind (L _ bindings) -> rebuildSetLayout ann kind bindings
  _ -> Left NotASet

-- | Rebuild the layout of a let-expression from its current children.
repairLetLayout :: Expr -> EditResult Expr
repairLetLayout = \case
  NixLet ann (L _ bindings) body -> rebuildLetLayout ann bindings body
  _ -> Left NotALet

-- | Rebuild the layout of a list expression from its current elements.
repairListLayout :: Expr -> EditResult Expr
repairListLayout = \case
  NixList ann xs -> rebuildListLayout ann xs
  _ -> Left NotAList

-- | Repair an expression after structural mutation.
--
-- Use this when the caller has already transformed an expression tree and now
-- wants to make it exact-printable again.
repairExprLayout :: Expr -> EditResult Expr
repairExprLayout expr = unLoc <$> runRepairAt (spanStartCursor (exprSpan expr)) (repairExpr (L (exprSpan expr) expr))

-- | Repair a binding after structural mutation.
repairBindingLayout :: Binding -> EditResult Binding
repairBindingLayout binding = unLoc <$> runRepairAt (spanStartCursor (bindingSpan binding)) (repairBinding (L (bindingSpan binding) binding))

-- | Repair an attribute path after structural mutation.
repairAttrPathLayout :: AttrPath -> EditResult AttrPath
repairAttrPathLayout path = unLoc <$> runRepairAt (spanStartCursor (attrPathSpan path)) (repairAttrPath (L (attrPathSpan path) path))

-- | Repair a function pattern after structural mutation.
repairFuncPatLayout :: FuncPat -> EditResult FuncPat
repairFuncPatLayout pat = unLoc <$> runRepairAt (spanStartCursor (funcPatBodySpan pat)) (repairFuncPat (L (funcPatBodySpan pat) pat))

-- Binding/list edit primitives

insertBindingIntoSet :: BindingInsertPosition -> AnnSet -> NixSetIsRecursive -> [LBinding] -> LBinding -> EditResult Expr
insertBindingIntoSet position ann kind bindings newBinding = do
  idx <- normalizeInsertIndex position (length bindings)
  rebuildSetLayout ann kind (insertAt idx newBinding bindings)

replaceBindingInSet :: Int -> AnnSet -> NixSetIsRecursive -> [LBinding] -> LBinding -> EditResult Expr
replaceBindingInSet idx ann kind bindings replacement
  | otherwise = do
      target <- elementAt idx bindings
      let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
      rebuildSetLayout ann kind (replaceAt idx replacement' bindings)

insertBindingIntoLet :: BindingInsertPosition -> AnnLetNode -> [LBinding] -> LExpr -> LBinding -> EditResult Expr
insertBindingIntoLet position ann bindings body newBinding = do
  idx <- normalizeInsertIndex position (length bindings)
  rebuildLetLayout ann (insertAt idx newBinding bindings) body

replaceBindingValueInBindings :: Int -> LExpr -> [LBinding] -> EditResult [LBinding]
replaceBindingValueInBindings idx replacementExpr bindings
  | otherwise = do
      target <- elementAt idx bindings
      replacement <- replaceBindingValueAt idx replacementExpr target
      pure (replaceAt idx replacement bindings)

replaceBindingValueAt :: Int -> LExpr -> LBinding -> EditResult LBinding
replaceBindingValueAt idx replacementExpr (L l binding) =
  case binding of
    NixNormalBinding ann path expr ->
      let translatedExpr = translateFromTo (getLoc replacementExpr) (getLoc expr) replacementExpr
          binding' = L l (NixNormalBinding ann path translatedExpr)
       in Right (normalizeBindingLayout binding')
    NixInheritBinding {} -> Left (NotANormalBinding idx)

renameBindingAttrPathKey :: Int -> Int -> LAttrKey -> [LBinding] -> EditResult [LBinding]
renameBindingAttrPathKey bindingIdx keyIdx replacementKey bindings
  | otherwise = do
      target <- elementAt bindingIdx bindings
      binding' <- renameAttrPathKeyInBinding bindingIdx keyIdx replacementKey target
      pure (replaceAt bindingIdx binding' bindings)

renameAttrPathKeyInBinding :: Int -> Int -> LAttrKey -> LBinding -> EditResult LBinding
renameAttrPathKeyInBinding bindingIdx keyIdx replacementKey (L l binding) =
  case binding of
    NixNormalBinding ann path expr -> do
      path' <- renameAttrPathKeyInPath bindingIdx keyIdx replacementKey path
      pure (normalizeBindingLayout (L l (NixNormalBinding ann path' expr)))
    NixInheritBinding {} -> Left (NotANormalBinding bindingIdx)

renameAttrPathKeyInPath :: Int -> Int -> LAttrKey -> LAttrPath -> EditResult LAttrPath
renameAttrPathKeyInPath _ keyIdx replacementKey (L l (NixAttrPath ann keys))
  | otherwise = do
      oldKey <- elementAt keyIdx keys
      let replacement' = translateFromTo (getLoc replacementKey) (getLoc oldKey) replacementKey
      Right (normalizeAttrPathLayout (L l (NixAttrPath ann (replaceAt keyIdx replacement' keys))))

--------------------------------------------------------------------------------

-- Small collection/index helpers

normalizeInsertIndex :: BindingInsertPosition -> Int -> EditResult Int
normalizeInsertIndex AppendBinding len = Right len
normalizeInsertIndex (InsertBindingAt idx) len
  | idx < 0 = Left (NegativeIndex idx)
  | idx > len = Left (IndexOutOfRange idx len)
  | otherwise = Right idx

normalizeListInsertIndex :: ListInsertPosition -> Int -> EditResult Int
normalizeListInsertIndex AppendListElement len = Right len
normalizeListInsertIndex (InsertListElementAt idx) len
  | idx < 0 = Left (NegativeIndex idx)
  | idx > len = Left (IndexOutOfRange idx len)
  | otherwise = Right idx

deleteAt :: Int -> [a] -> EditResult [a]
deleteAt idx xs
  | otherwise = elementAt idx xs >> Right (take idx xs <> drop (idx + 1) xs)

elementAt :: Int -> [a] -> EditResult a
elementAt idx xs
  | idx < 0 = Left (NegativeIndex idx)
  | idx >= length xs = Left (IndexOutOfRange idx (length xs))
  | otherwise = Right (xs !! idx)

insertAt :: Int -> a -> [a] -> [a]
insertAt idx x xs = take idx xs <> [x] <> drop idx xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx x xs = take idx xs <> [x] <> drop (idx + 1) xs

--------------------------------------------------------------------------------

-- Recursive repair walkers

-- | Repair an expression subtree starting from the current repair cursor.
--
-- This is the main recursive dispatcher for exact-print repair. Each case keeps
-- the original node shape, recomputes the concrete spans of children, and then
-- retags the node annotations so the tree becomes exact-printable again.
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

-- | Re-anchor an expression at a concrete cursor and then repair it recursively.
--
-- The translation step keeps the subtree's internal relative layout intact while
-- moving its outer anchor to the requested cursor. Recursive repair then fixes
-- any token annotations that are defined relative to child layout.
repairExprAt :: RenderCursor -> LExpr -> RepairM LExpr
repairExprAt cursor expr = withRepairCursor cursor (repairExpr translated)
  where
    translated = translateFromTo (getLoc expr) (cursorSpan cursor (srcSpanFilename (getLoc expr))) expr

-- | Repair a child node at a cursor derived from an old/new anchor pair.
repairChildAfter :: SrcSpan -> SrcSpan -> SrcSpan -> LExpr -> RepairM LExpr
repairChildAfter oldAnchor oldTarget newAnchor child =
  repairExprAt (preserveGapTarget oldAnchor oldTarget newAnchor) child

-- | Repair a variable expression by re-spanning its identifier.
repairVarExpr :: SrcSpan -> AnnCommon -> LId -> LExpr
repairVarExpr originalSpan ann ident =
  let ident' = repairLocatedIdAt (spanStartCursor originalSpan) ident
      span' = getLoc ident'
      ann' = setAnnSpan span' ann
   in L span' (NixVar ann' ident')

-- | Repair a literal expression by recomputing the literal leaf span.
repairLit :: SrcSpan -> AnnCommon -> LLit -> LExpr
repairLit originalSpan ann lit =
  let lit' = repairLocatedLitAt (spanStartCursor originalSpan) lit
      span' = getLoc lit'
      ann' = setAnnSpan span' ann
   in L span' (NixLit ann' lit')

-- | Repair a string expression by recomputing the leaf string span.
repairStringExpr :: SrcSpan -> AnnStringNode -> LNString -> LExpr
repairStringExpr originalSpan ann str =
  let str' = repairLocatedStringAt (spanStartCursor originalSpan) str
      span' = getLoc str'
      ann' = setAnnSpan span' ann
   in L span' (NixString ann' str')

-- | Repair a path expression by recomputing the leaf path span.
repairPathExpr :: SrcSpan -> AnnPathNode -> LPath -> LExpr
repairPathExpr originalSpan ann path =
  let path' = repairLocatedPathAt (spanStartCursor originalSpan) path
      span' = getLoc path'
      ann' = setAnnSpan span' ann
   in L span' (NixPath ann' path')

-- | Repair a parenthesized expression.
--
-- Algorithm:
--
-- * place the opening token at the original outer cursor,
-- * repair the inner expression relative to the new opening token,
-- * preserve the close-paren gap relative to the repaired child,
-- * normalize the paren annotation so exact printing uses the repaired deltas.
repairPar :: SrcSpan -> AnnParNode -> LExpr -> RepairM LExpr
repairPar originalSpan ann inner = do
  let openSpan = tokenSpanAt (spanStartCursor originalSpan) (apnOpenP ann)
  inner' <- repairChildAfter (expectTokenSpan "paren open" (apnOpenP ann)) (getLoc inner) openSpan inner
  let closeSpan = preserveGapSpan (getLoc inner) (expectTokenSpan "paren close" (apnCloseP ann)) inner'
      ann0 = ann {apnOpenP = (apnOpenP ann) {annTokenPos = AnnSpan openSpan}, apnCloseP = (apnCloseP ann) {annTokenPos = AnnSpan closeSpan}}
      ann' = setAnnSpan (openSpan `combineSrcSpans` closeSpan) (prepareParLayout ann0 (unLoc inner'))
      expr' = NixPar ann' inner'
  pure (L (exprSpan expr') expr')

-- | Repair an environment-path expression while preserving the literal source text.
repairEnvPath :: SrcSpan -> AnnEnvPathNode -> Located Text -> LExpr
repairEnvPath _ ann path =
  let openSpan = expectTokenSpan "env path open" (aenvOpen ann)
      path' = repairLocatedTextAt (spanStartCursor (preserveGapSpan openSpan (getLoc path) (L openSpan ()))) path
      closeSpan = preserveGapSpan (getLoc path) (expectTokenSpan "env path close" (aenvClose ann)) path'
      ann' = setAnnSpan (openSpan `combineSrcSpans` closeSpan) ann {aenvClose = (aenvClose ann) {annTokenPos = AnnSpan closeSpan}}
      span' = openSpan `combineSrcSpans` closeSpan
   in L span' (NixEnvPath ann' path')

-- | Repair a lambda expression by preserving the colon gap between pattern and body.
repairLam :: SrcSpan -> AnnLamNode -> LFuncPat -> LExpr -> RepairM LExpr
repairLam originalSpan ann pat body = do
  pat' <- repairFuncPatAt (spanStartCursor originalSpan) pat
  let colonSpan = preserveGapSpan (funcPatRenderSpan (unLoc pat)) (expectTokenSpan "lambda colon" (alamColon ann)) pat'
  body' <- repairChildAfter (expectTokenSpan "lambda colon" (alamColon ann)) (getLoc body) colonSpan body
  let span' = funcPatRenderSpan (unLoc pat') `combineSrcSpans` getLoc body'
      ann' = setAnnSpan span' ann {alamColon = (alamColon ann) {annTokenPos = AnnSpan colonSpan}}
  pure (L span' (NixLam ann' pat' body'))

-- | Repair application by anchoring the left child first, then preserving the lhs-rhs gap.
repairApp :: SrcSpan -> AnnAppNode -> LExpr -> LExpr -> RepairM LExpr
repairApp originalSpan ann lhs rhs = do
  lhs' <- repairExprAt (spanStartCursor originalSpan) lhs
  rhs' <- repairChildAfter (getLoc lhs) (getLoc rhs) (getLoc lhs') rhs
  let span' = getLoc lhs' `combineSrcSpans` getLoc rhs'
      ann' = setAnnSpan span' ann
  pure (L span' (NixApp ann' lhs' rhs'))

-- | Repair a binary application by preserving the operator and rhs gaps.
repairBinApp :: SrcSpan -> AnnBinAppNode -> BinaryOp -> LExpr -> LExpr -> RepairM LExpr
repairBinApp originalSpan ann op lhs rhs = do
  lhs' <- repairExprAt (spanStartCursor originalSpan) lhs
  let opSpan = preserveGapSpan (getLoc lhs) (expectTokenSpan "binary operator" (abinOperator ann)) lhs'
  rhs' <- repairChildAfter (expectTokenSpan "binary operator" (abinOperator ann)) (getLoc rhs) opSpan rhs
  let span' = getLoc lhs' `combineSrcSpans` getLoc rhs'
      ann' = setAnnSpan span' ann {abinOperator = (abinOperator ann) {annTokenPos = AnnSpan opSpan}}
  pure (L span' (NixBinApp ann' op lhs' rhs'))

-- | Repair a prefix operator expression.
--
-- The operator token is placed first, then the operand is repaired relative to
-- that new operator span.
repairPrefix :: SrcSpan -> AnnPrefixNode -> (AnnPrefixNode -> LExpr -> Expr) -> LExpr -> RepairM LExpr
repairPrefix originalSpan ann mkNode inner = do
  let tokSpan = tokenSpanAt (spanStartCursor originalSpan) (apfxToken ann)
  inner' <- repairChildAfter (expectTokenSpan "prefix operator" (apfxToken ann)) (getLoc inner) tokSpan inner
  let span' = tokSpan `combineSrcSpans` getLoc inner'
      ann' = setAnnSpan span' ann {apfxToken = (apfxToken ann) {annTokenPos = AnnSpan tokSpan}}
  pure (L span' (mkNode ann' inner'))

-- | Repair a list by recursively repairing elements and then delegating final spacing to rebuild.
repairList :: AnnListNode -> [LExpr] -> RepairM LExpr
repairList ann xs = do
  xs' <- mapM repairExpr xs
  repaired <- liftEditResult (rebuildListLayout ann xs')
  pure (L (exprSpan repaired) repaired)

-- | Repair a set by recursively repairing bindings and then rebuilding container layout.
repairSet :: AnnSet -> NixSetIsRecursive -> [LBinding] -> RepairM LExpr
repairSet ann kind bindings = do
  bindings' <- mapM repairBinding bindings
  repaired <- liftEditResult (rebuildSetLayout ann kind bindings')
  pure (L (exprSpan repaired) repaired)

-- | Repair a @let@ by recursively repairing bindings and body and then rebuilding layout.
repairLet :: AnnLetNode -> [LBinding] -> LExpr -> RepairM LExpr
repairLet ann bindings body = do
  bindings' <- mapM repairBinding bindings
  body' <- repairExpr body
  repaired <- liftEditResult (rebuildLetLayout ann bindings' body')
  pure (L (exprSpan repaired) repaired)

-- | Repair a @hasAttr@ expression by preserving the question-mark gap.
repairHasAttr :: SrcSpan -> AnnHasAttr -> LExpr -> LAttrPath -> RepairM LExpr
repairHasAttr originalSpan ann lhs path = do
  lhs' <- repairExprAt (spanStartCursor originalSpan) lhs
  let qSpan = preserveGapSpan (getLoc lhs) (expectTokenSpan "has-attr question" (ahaQuestion ann)) lhs'
  path' <- repairAttrPathAt (preserveGapTarget (expectTokenSpan "has-attr question" (ahaQuestion ann)) (getLoc path) qSpan) path
  let span' = getLoc lhs' `combineSrcSpans` attrPathSpan (unLoc path')
      ann' = setAnnSpan span' (prepareHasAttrLayout (ann {ahaQuestion = (ahaQuestion ann) {annTokenPos = AnnSpan qSpan}}) (unLoc lhs') (unLoc path'))
  pure (L span' (NixHasAttr ann' lhs' path'))

-- | Repair a selection expression, including optional @or@ default layout.
repairSelect :: SrcSpan -> AnnSelect -> LExpr -> LAttrPath -> Maybe LExpr -> RepairM LExpr
repairSelect originalSpan ann lhs path def = do
  lhs' <- repairExprAt (spanStartCursor originalSpan) lhs
  path' <- repairAttrPathAt (preserveGapTarget (getLoc lhs) (getLoc path) (getLoc lhs')) path
  def' <- traverse (repairExprAtSelect ann path path') def
  let annWithOr = case (aslOr ann, def, def') of
        (Just orTok, Just _, Just _) ->
          let orSpan = preserveGapSpan (attrPathRenderSpan (unLoc path)) (expectTokenSpan "select or" orTok) path'
           in ann {aslOr = Just (orTok {annTokenPos = AnnSpan orSpan})}
        _ -> ann
      ann' = setAnnSpan finalSpan (prepareSelectLayout annWithOr (unLoc lhs') (unLoc path') def')
      finalSpan = maybe baseSpan (combineSrcSpans baseSpan . getLoc) def'
      baseSpan = getLoc lhs' `combineSrcSpans` attrPathSpan (unLoc path')
  pure (L finalSpan (NixSelect ann' lhs' path' def'))

-- | Repair an @if ... then ... else ...@ chain by preserving keyword-to-branch gaps.
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

-- | Repair a @with ...; ...@ expression by preserving the semicolon split.
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

-- | Repair an @assert ...; ...@ expression by preserving the semicolon split.
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

-- Binding, attr-path, and function-pattern repair walkers

-- | Repair a binding subtree from the current repair cursor.
repairBinding :: LBinding -> RepairM LBinding
repairBinding (L originalSpan node) =
  case node of
    NixNormalBinding ann path expr -> repairNormalBinding originalSpan ann path expr
    NixInheritBinding ann mScope names -> repairInheritBinding originalSpan ann mScope names

-- | Repair a normal @path = expr;@ binding.
--
-- The attribute path is repaired first, then the equals and semicolon tokens are
-- re-anchored relative to the repaired children.
repairNormalBinding :: SrcSpan -> AnnNormalBinding -> LAttrPath -> LExpr -> RepairM LBinding
repairNormalBinding originalSpan ann path expr = do
  path' <- repairAttrPathAt (spanStartCursor originalSpan) path
  let eqSpan = preserveGapSpan (attrPathSpan (unLoc path)) (expectTokenSpan "binding equals" (anbEqual ann)) path'
  expr' <- repairExprAt (preserveGapTarget (expectTokenSpan "binding equals" (anbEqual ann)) (getLoc expr) eqSpan) expr
  let semiSpan = preserveGapSpan (getLoc expr) (expectTokenSpan "binding semicolon" (anbSemicolon ann)) expr'
      ann' = setAnnSpan span' ann {anbEqual = (anbEqual ann) {annTokenPos = AnnSpan eqSpan}, anbSemicolon = (anbSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
      span' = attrPathSpan (unLoc path') `combineSrcSpans` semiSpan
  pure (L span' (NixNormalBinding ann' path' expr'))

-- | Repair an @inherit@ binding while preserving optional scope and trailing names.
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

-- | Repair an attribute path starting at its own render span.
repairAttrPath :: LAttrPath -> RepairM LAttrPath
repairAttrPath path = repairAttrPathAt (spanStartCursor (getLoc path)) path

-- | Repair an attribute path at a given cursor.
--
-- The current cursor is stored in 'RepairContext' so nested helpers can reuse it
-- without re-threading the same value through every intermediate call.
repairAttrPathAt :: RenderCursor -> LAttrPath -> RepairM LAttrPath
repairAttrPathAt cursor (L _ (NixAttrPath ann keys)) =
  withRepairCursor cursor $
    case keys of
      [] -> liftEditResult (Left EmptyAttrPathEdit)
      firstKey : restKeys -> repairAttrPathHead ann keys firstKey restKeys

-- | Repair the head of an attribute path and then walk the remaining dotted tail.
--
-- This handles both normal and "leading dot" forms by deciding whether the
-- first concrete token belongs to a dot or a key.
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

-- | Repair a function pattern starting at its own render span.
repairFuncPat :: LFuncPat -> RepairM LFuncPat
repairFuncPat pat = repairFuncPatAt (spanStartCursor (getLoc pat)) pat

-- | Repair a function pattern at a given cursor.
repairFuncPatAt :: RenderCursor -> LFuncPat -> RepairM LFuncPat
repairFuncPatAt cursor (L _ node) =
  withRepairCursor cursor $
    case node of
      NixVarPat ann ident ->
        let ident' = repairLocatedIdAt cursor ident
            span' = getLoc ident'
            ann' = setAnnSpan span' ann {avpId = span'}
         in pure (L span' (NixVarPat ann' ident'))
      NixSetPat ann ellipses mAs bindings -> repairSetPat ann ellipses mAs bindings

-- | Repair a set-pattern function argument.
--
-- This places the opening token, repairs any @as@ pattern, then repairs the
-- binding sequence left-to-right before recomputing the close token.
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

-- | Choose where entries in a set pattern begin after placing the opening token.
setPatEntryCursor :: SrcSpan -> Maybe LSetPatAs -> RenderCursor
setPatEntryCursor openSpan = \case
  Just asPat@(L _ NixSetPatAs {nspaLocation = NixSetPatAsLeading}) -> spanStartCursor (getLoc asPat)
  _ -> RenderCursor (srcSpanStartLine openSpan) (srcSpanEndColumn openSpan)

-- | Repair a single attribute key at a concrete cursor.
repairAttrKeyAt :: RenderCursor -> LAttrKey -> RepairM LAttrKey
repairAttrKeyAt cursor (L _ key) =
  let span' = case key of
        NixStaticAttrKey _ ident -> textSpanAt (cursorFile ident) cursor (unLoc ident)
        NixDynamicStringAttrKey _ _ -> textSpanAt cursorFallbackFile cursor (renderAttrKeyTextLocal key)
        NixDynamicInterpolAttrKey _ _ -> textSpanAt cursorFallbackFile cursor (renderAttrKeyTextLocal key)
      key' = case key of
        NixStaticAttrKey ann ident -> NixStaticAttrKey ann (repairLocatedIdAt cursor ident)
        other -> other
   in pure (L span' key')

--------------------------------------------------------------------------------

-- Leaf repair helpers

-- | Repair a literal leaf by assigning it the span of its rendered text.
repairLocatedLitAt :: RenderCursor -> LLit -> LLit
repairLocatedLitAt cursor (L _ lit) = L (textSpanAt cursorFallbackFile cursor (renderLitTextLocal lit)) lit

--------------------------------------------------------------------------------

-- | Repair a string leaf by assigning it the span of its rendered source text.
repairLocatedStringAt :: RenderCursor -> LNString -> LNString
repairLocatedStringAt cursor (L _ str) = L (textSpanAt cursorFallbackFile cursor (renderStringTextLocal str)) str

--------------------------------------------------------------------------------

-- | Repair a path leaf by assigning it the span of its rendered source text.
repairLocatedPathAt :: RenderCursor -> LPath -> LPath
repairLocatedPathAt cursor (L _ path) = L (textSpanAt cursorFallbackFile cursor (renderPathTextLocal path)) path

--------------------------------------------------------------------------------

-- | Repair a raw text leaf by assigning it the span of its rendered text.
repairLocatedTextAt :: RenderCursor -> Located Text -> Located Text
repairLocatedTextAt cursor (L _ txt) = L (textSpanAt cursorFallbackFile cursor txt) txt

--------------------------------------------------------------------------------

-- | Repair an identifier leaf by assigning it the span of its rendered text.
repairLocatedIdAt :: RenderCursor -> LId -> LId
repairLocatedIdAt cursor (L _ ident) = L (textSpanAt cursorFallbackFile cursor ident) ident

--------------------------------------------------------------------------------

-- | Repair an @as@ fragment inside a set pattern.
repairSetPatAsAt :: RenderCursor -> LSetPatAs -> RepairM LSetPatAs
repairSetPatAsAt cursor (L _ asPat@NixSetPatAs {..}) =
  let var' = repairLocatedIdAt cursor nspaVar
      atSpan = case nspaLocation of
        NixSetPatAsLeading -> tokenSpanAt (spanEndCursor (getLoc var')) (aspaAt nspaAnn)
        NixSetPatAsTrailing -> tokenSpanAt cursor (aspaAt nspaAnn)
      span' = setPatAsRenderSpan (nspaAnn {aspaAt = (aspaAt nspaAnn) {annTokenPos = AnnSpan atSpan}}) var' nspaLocation
      ann' = setAnnSpan span' nspaAnn {aspaAt = (aspaAt nspaAnn) {annTokenPos = AnnSpan atSpan}}
   in pure (L span' asPat {nspaAnn = ann', nspaVar = var'})

-- | Repair set-pattern bindings from left to right.
--
-- This is one of the few places where cursor progression is intentionally kept
-- explicit rather than hidden in 'RepairContext', because the algorithm is a
-- sequence fold that advances after each repaired binding.
repairSetPatBindingsAt :: AnnSetPatNode -> RenderCursor -> [LSetPatBinding] -> RepairM [LSetPatBinding]
repairSetPatBindingsAt ann startCursor bindings = snd <$> foldM step (startCursor, []) (zip [0 ..] bindings)
  where
    step (cursor, repaired) (idx, binding) = do
      binding' <- repairSetPatBindingAt ann idx cursor binding
      let next = advanceCursor (spanStartCursor (getLoc binding')) (renderSetPatBindingSyntax (unLoc binding'))
      pure (next, repaired <> [binding'])

-- | Repair a single set-pattern binding, including its optional default value.
repairSetPatBindingAt :: AnnSetPatNode -> Int -> RenderCursor -> LSetPatBinding -> RepairM LSetPatBinding
repairSetPatBindingAt _ _ cursor (L _ binding@NixSetPatBinding {..}) = do
  let var' = repairLocatedIdAt cursor nspbVar
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

-- | Repair the dotted tail of an attribute path.
--
-- The algorithm alternates between preserving a dot token gap and repairing the
-- next key relative to that new dot span.
repairAttrTail :: [AnnToken] -> [LAttrKey] -> SrcSpan -> LAttrKey -> RepairM ([AnnToken], [LAttrKey])
repairAttrTail [] [] _ _ = pure ([], [])
repairAttrTail (dotTok : dotToks) (key : keys) oldPrev newPrev = do
  let dotSpan = preserveGapSpan oldPrev (expectTokenSpan "attr path dot" dotTok) (L (getLoc newPrev) ())
  key' <- repairAttrKeyAt (preserveGapTarget dotSpan (getLoc key) dotSpan) key
  (dots', keys') <- repairAttrTail dotToks keys (getLoc key) key'
  pure (dotTok {annTokenPos = AnnSpan dotSpan} : dots', key' : keys')
repairAttrTail dots keys _ _ = liftEditResult (Left (IndexOutOfRange (length dots) (length keys)))

--------------------------------------------------------------------------------

-- | Repair the names inside an @inherit@ binding from left to right.
repairInheritNames :: SrcSpan -> Maybe (LExpr) -> Maybe (LExpr) -> [LAttrKey] -> RepairM [LAttrKey]
repairInheritNames inheritSpan oldScope newScope names = snd <$> foldM step (startAnchor, []) names
  where
    startAnchor = maybe inheritSpan getLoc newScope
    oldStartAnchor = maybe inheritSpan getLoc oldScope
    step (prevNew, repaired) name = do
      let oldPrev = if null repaired then oldStartAnchor else getLoc (last names)
      name' <- repairAttrKeyAt (preserveGapTarget oldPrev (getLoc name) prevNew) name
      pure (getLoc name', repaired <> [name'])

--------------------------------------------------------------------------------

-- | Repair the default expression of a selection after its attr path changes.
repairExprAtSelect :: AnnSelect -> LAttrPath -> LAttrPath -> LExpr -> RepairM LExpr
repairExprAtSelect ann oldPath newPath oldDef =
  case aslOr ann of
    Just orTok ->
      let orSpan = preserveGapSpan (attrPathRenderSpan (unLoc oldPath)) (expectTokenSpan "select or" orTok) newPath
       in repairExprAt (preserveGapTarget (expectTokenSpan "select or" orTok) (getLoc oldDef) orSpan) oldDef
    Nothing -> repairExpr oldDef

-- | Lift a plain 'EditResult' action into the internal repair reader.
liftEditResult :: EditResult a -> RepairM a
liftEditResult = ReaderT . const

--------------------------------------------------------------------------------

-- | Compute the closing brace span of a repaired set pattern.
repairSetPatCloseSpan :: AnnSetPatNode -> [LSetPatBinding] -> NixSetPatEllipses -> SrcSpan
repairSetPatCloseSpan ann bindings _ =
  case reverse bindings of
    lastBinding : _ ->
      let endCursor = advanceCursor (spanStartCursor (getLoc lastBinding)) (renderSetPatBindingSyntax (unLoc lastBinding))
       in tokenSpanAt endCursor (aspCloseC ann)
    [] -> expectTokenSpan "set pattern close" (aspCloseC ann)

--------------------------------------------------------------------------------

-- | Render a set-pattern binding for cursor advancement.
renderSetPatBindingSyntax :: SetPatBinding -> Text
renderSetPatBindingSyntax = renderStrict . layoutPretty defaultLayoutOptions . output

--------------------------------------------------------------------------------

-- Shared rendering, spacing, and translation helpers

-- | Render a literal the same way exact-print leaf repair expects to see it.
renderLitTextLocal :: Lit -> Text
renderLitTextLocal = \case
  NixUri _ uri -> uri
  NixInteger _ int -> T.pack (show int)
  NixFloat _ float -> T.pack (show float)
  NixBoolean _ True -> "true"
  NixBoolean _ False -> "false"
  NixNull _ -> "null"

-- | Render a string node using its preserved source text.
renderStringTextLocal :: NString -> Text
renderStringTextLocal = \case
  NixDoubleQuotesString (SourceText src) _ -> "\"" <> src <> "\""
  NixDoubleSingleQuotesString (SourceText src) _ -> "''" <> src <> "''"

-- | Render a path node using its preserved source text.
renderPathTextLocal :: Path -> Text
renderPathTextLocal = \case
  NixLiteralPath _ path -> path
  NixInterpolPath (SourceText src) _ -> src

-- | Render an attribute key using preserved source text where possible.
renderAttrKeyTextLocal :: AttrKey -> Text
renderAttrKeyTextLocal = \case
  NixStaticAttrKey _ ident -> unLoc ident
  NixDynamicStringAttrKey (SourceText src) _ -> "\"" <> src <> "\""
  NixDynamicInterpolAttrKey (SourceText src) _ -> "${" <> src <> "}"

--------------------------------------------------------------------------------
