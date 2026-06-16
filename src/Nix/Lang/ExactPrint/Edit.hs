{-# LANGUAGE RankNTypes #-}

-- | Low-level exact-print editing and repair operations.
--
-- The exported @repair*Layout@ functions are the public APIs for
-- repairing AST nodes after structural mutation so they can be exact-printed
-- again.
--
-- Internally, this module also contains @rebuild*Layout@ helpers. Those are
-- container-specific primitives for reconstructing set/let/list layout from
-- existing child nodes, anchor choices, and boundary comments.
--
-- The implementation is organized in layers:
--
-- * typed fragment parsers and typed edit primitives over sets, lets, and lists
-- * text convenience wrappers built on those typed operations
-- * public structural repair entry points
-- * container-specific rebuild primitives
-- * recursive repair walkers for expressions, bindings, attribute paths, and
--   function patterns
-- * low-level cursor/span utilities used by rebuild and repair logic
module Nix.Lang.ExactPrint.Edit
  ( EditError (..),
    BindingInsertPosition (..),
    ListInsertPosition (..),
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

import Data.Data (Data)
import Data.Generics (everywhere, mkT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Outputable (output)
import Nix.Lang.Parser (Parser, attrKey, located, nixBinding, nixExpr, runNixParser)
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec (eof, errorBundlePretty)

--------------------------------------------------------------------------------

-- Public types

type EditM = Either EditError

data EditError
  = NotASet
  | NotALet
  | NotAList
  | NotABindingContainer
  | EmptyAttrPathEdit
  | NotANormalBinding Int
  | NegativeIndex Int
  | IndexOutOfRange Int Int
  | ParseBindingError Text
  | ParseExprError Text
  | ParseAttrKeyError Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------

data BindingInsertPosition
  = InsertBindingAt Int
  | AppendBinding
  deriving (Show, Eq)

data ListInsertPosition
  = InsertListElementAt Int
  | AppendListElement
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- Public span/comment queries

bindingSpan :: Binding -> SrcSpan
bindingSpan = \case
  NixNormalBinding ann path expr ->
    fromMaybe fallback $ do
      equalSpan <- annTokenSrcSpan (anbEqual ann)
      semicolonSpan <- annTokenSrcSpan (anbSemicolon ann)
      pure $ attrPathSpan (unLoc path) `combineSrcSpans` equalSpan `combineSrcSpans` getLoc expr `combineSrcSpans` semicolonSpan
    where
      fallback = fromMaybe (attrPathSpan (unLoc path) `combineSrcSpans` getLoc expr) (annSrcSpan ann)
  NixInheritBinding ann mScope names ->
    fromMaybe fallback $ do
      inheritSpan <- annTokenSrcSpan (aibInherit ann)
      semicolonSpan <- annTokenSrcSpan (aibSemicolon ann)
      let base = inheritSpan `combineSrcSpans` semicolonSpan
      pure $ foldr combineSrcSpans base (maybe [] ((: []) . getLoc) mScope <> fmap getLoc names)
    where
      fallback = fromMaybe (foldr combineSrcSpans (mkSrcSpan "<edited>" (1, 1) (1, 1)) (maybe [] ((: []) . getLoc) mScope <> fmap getLoc names)) (annSrcSpan ann)

bindingRenderSpan :: Binding -> SrcSpan
bindingRenderSpan binding =
  foldr combineSrcSpans (bindingSpan binding) (getLoc <$> priorComments (bindingComments binding))

bindingComments :: Binding -> NodeComments
bindingComments = \case
  NixNormalBinding ann _ _ -> annComments ann
  NixInheritBinding ann _ _ -> annComments ann

--------------------------------------------------------------------------------

-- Public edit operations

-- | Parse a standalone expression fragment.
parseExprFragment :: Text -> EditM LExpr
parseExprFragment = parseLocatedFragment "<expr>" locatedExprParser ParseExprError
  where
    locatedExprParser = do
      L l expr <- located nixExpr
      pure (L l expr)

-- | Parse a standalone binding fragment.
parseBindingFragment :: Text -> EditM LBinding
parseBindingFragment = parseLocatedFragment "<binding>" locatedBindingParser ParseBindingError
  where
    locatedBindingParser = do
      L l binding <- located nixBinding
      pure (L l binding)

-- | Parse a standalone attribute-key fragment.
parseAttrKeyFragment :: Text -> EditM LAttrKey
parseAttrKeyFragment = parseLocatedFragment "<attr-key>" locatedKeyParser ParseAttrKeyError
  where
    locatedKeyParser = do
      L l key <- located attrKey
      pure (L l key)

-- | Insert a parsed binding node into a set expression.
insertSetBindingNodeAt :: BindingInsertPosition -> LBinding -> Expr -> EditM Expr
insertSetBindingNodeAt position binding = \case
  NixSet ann kind (L _ bindings) -> insertBindingIntoSet position ann kind bindings binding
  _ -> Left NotASet

-- | Replace a set binding with a parsed binding node.
replaceSetBindingNodeAt :: Int -> LBinding -> Expr -> EditM Expr
replaceSetBindingNodeAt idx replacement = \case
  NixSet ann kind (L _ bindings) -> replaceBindingInSet idx ann kind bindings replacement
  _ -> Left NotASet

-- | Insert a parsed binding node into a set or let binding container.
insertBindingNodeAt :: BindingInsertPosition -> LBinding -> Expr -> EditM Expr
insertBindingNodeAt position binding expr =
  case expr of
    NixSet ann kind (L _ bindings) -> insertBindingIntoSet position ann kind bindings binding
    NixLet ann (L _ bindings) body -> insertBindingIntoLet position ann bindings body binding
    _ -> Left NotABindingContainer

-- | Replace a binding with a parsed binding node in a set or let container.
replaceBindingNodeAt :: Int -> LBinding -> Expr -> EditM Expr
replaceBindingNodeAt idx replacement expr =
  case expr of
    NixSet ann kind (L _ bindings) -> replaceBindingInSet idx ann kind bindings replacement
    NixLet ann (L _ bindings) body -> do
      target <- elementAt idx bindings
      let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
      rebuildLetLayout ann (replaceAt idx replacement' bindings) body
    _ -> Left NotABindingContainer

-- | Replace the RHS of a normal binding with a parsed expression node.
replaceBindingValueNodeAt :: Int -> LExpr -> Expr -> EditM Expr
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
insertListElementNodeAt :: ListInsertPosition -> LExpr -> Expr -> EditM Expr
insertListElementNodeAt position element expr =
  case expr of
    NixList ann xs -> do
      idx <- normalizeListInsertIndex position (length xs)
      rebuildListLayout ann (insertAt idx element xs)
    _ -> Left NotAList

-- | Replace a list element with a parsed expression node.
replaceListElementNodeAt :: Int -> LExpr -> Expr -> EditM Expr
replaceListElementNodeAt idx replacement expr =
  case expr of
    NixList ann xs -> do
      target <- elementAt idx xs
      let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
      rebuildListLayout ann (replaceAt idx replacement' xs)
    _ -> Left NotAList

insertSetBindingText :: BindingInsertPosition -> Text -> Expr -> EditM Expr
insertSetBindingText position bindingText expr = do
  newBinding <- parseBindingFragment bindingText
  insertSetBindingNodeAt position newBinding expr

insertBindingAt :: BindingInsertPosition -> Text -> Expr -> EditM Expr
insertBindingAt position bindingText expr = do
  binding <- parseBindingFragment bindingText
  insertBindingNodeAt position binding expr

deleteBindingAt :: Int -> Expr -> EditM Expr
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

replaceSetBindingText :: Int -> Text -> Expr -> EditM Expr
replaceSetBindingText idx bindingText expr = do
  replacement <- parseBindingFragment bindingText
  replaceSetBindingNodeAt idx replacement expr

replaceBindingValue :: Int -> Text -> Expr -> EditM Expr
replaceBindingValue idx valueText expr = do
  replacementExpr <- parseExprFragment valueText
  replaceBindingValueNodeAt idx replacementExpr expr

insertListElementAt :: ListInsertPosition -> Text -> Expr -> EditM Expr
insertListElementAt position elemText expr = do
  element <- parseExprFragment elemText
  insertListElementNodeAt position element expr

deleteListElementAt :: Int -> Expr -> EditM Expr
deleteListElementAt idx expr
  | idx < 0 = Left (NegativeIndex idx)
  | otherwise = case expr of
      NixList ann xs -> do
        xs' <- deleteAt idx xs
        rebuildListLayoutWithAnchor AnchorStartAtFirstSlot ann xs'
      _ -> Left NotAList

renameAttrPathKey :: Int -> Int -> Text -> Expr -> EditM Expr
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
repairSetLayout :: Expr -> EditM Expr
repairSetLayout = \case
  NixSet ann kind (L _ bindings) -> rebuildSetLayout ann kind bindings
  _ -> Left NotASet

-- | Rebuild the layout of a let-expression from its current children.
repairLetLayout :: Expr -> EditM Expr
repairLetLayout = \case
  NixLet ann (L _ bindings) body -> rebuildLetLayout ann bindings body
  _ -> Left NotALet

-- | Rebuild the layout of a list expression from its current elements.
repairListLayout :: Expr -> EditM Expr
repairListLayout = \case
  NixList ann xs -> rebuildListLayout ann xs
  _ -> Left NotAList

-- | Repair an expression after structural mutation.
--
-- Use this when the caller has already transformed an expression tree and now
-- wants to make it exact-printable again.
repairExprLayout :: Expr -> EditM Expr
repairExprLayout expr = unLoc <$> repairLocatedExpr (L (exprSpan expr) expr)

-- | Repair a binding after structural mutation.
repairBindingLayout :: Binding -> EditM Binding
repairBindingLayout binding = unLoc <$> repairLocatedBinding (L (bindingSpan binding) binding)

-- | Repair an attribute path after structural mutation.
repairAttrPathLayout :: AttrPath -> EditM AttrPath
repairAttrPathLayout path = unLoc <$> repairLocatedAttrPath (L (attrPathSpan path) path)

-- | Repair a function pattern after structural mutation.
repairFuncPatLayout :: FuncPat -> EditM FuncPat
repairFuncPatLayout pat = unLoc <$> repairLocatedFuncPat (L (funcPatBodySpan pat) pat)

--------------------------------------------------------------------------------

-- Fragment parsing helpers

parseLocatedFragment :: String -> Parser a -> (Text -> EditError) -> Text -> EditM a
parseLocatedFragment label parser mkErr src =
  case runNixParser (parser <* eof) label src of
    (Right value, _) -> Right value
    (Left err, _) -> Left (mkErr (T.pack (errorBundlePretty err)))

--------------------------------------------------------------------------------

-- Binding/list edit primitives

insertBindingIntoSet :: BindingInsertPosition -> AnnSet -> NixSetIsRecursive -> [LBinding] -> LBinding -> EditM Expr
insertBindingIntoSet position ann kind bindings newBinding = do
  idx <- normalizeInsertIndex position (length bindings)
  rebuildSetLayout ann kind (insertAt idx newBinding bindings)

replaceBindingInSet :: Int -> AnnSet -> NixSetIsRecursive -> [LBinding] -> LBinding -> EditM Expr
replaceBindingInSet idx ann kind bindings replacement
  | otherwise = do
      target <- elementAt idx bindings
      let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
      rebuildSetLayout ann kind (replaceAt idx replacement' bindings)

insertBindingIntoLet :: BindingInsertPosition -> AnnLetNode -> [LBinding] -> LExpr -> LBinding -> EditM Expr
insertBindingIntoLet position ann bindings body newBinding = do
  idx <- normalizeInsertIndex position (length bindings)
  rebuildLetLayout ann (insertAt idx newBinding bindings) body

replaceBindingValueInBindings :: Int -> LExpr -> [LBinding] -> EditM [LBinding]
replaceBindingValueInBindings idx replacementExpr bindings
  | otherwise = do
      target <- elementAt idx bindings
      replacement <- replaceBindingValueAt idx replacementExpr target
      pure (replaceAt idx replacement bindings)

replaceBindingValueAt :: Int -> LExpr -> LBinding -> EditM LBinding
replaceBindingValueAt idx replacementExpr (L l binding) =
  case binding of
    NixNormalBinding ann path expr ->
      let translatedExpr = translateFromTo (getLoc replacementExpr) (getLoc expr) replacementExpr
          binding' = L l (NixNormalBinding ann path translatedExpr)
       in Right (normalizeBindingLayout binding')
    NixInheritBinding {} -> Left (NotANormalBinding idx)

renameBindingAttrPathKey :: Int -> Int -> LAttrKey -> [LBinding] -> EditM [LBinding]
renameBindingAttrPathKey bindingIdx keyIdx replacementKey bindings
  | otherwise = do
      target <- elementAt bindingIdx bindings
      binding' <- renameAttrPathKeyInBinding bindingIdx keyIdx replacementKey target
      pure (replaceAt bindingIdx binding' bindings)

renameAttrPathKeyInBinding :: Int -> Int -> LAttrKey -> LBinding -> EditM LBinding
renameAttrPathKeyInBinding bindingIdx keyIdx replacementKey (L l binding) =
  case binding of
    NixNormalBinding ann path expr -> do
      path' <- renameAttrPathKeyInPath bindingIdx keyIdx replacementKey path
      pure (normalizeBindingLayout (L l (NixNormalBinding ann path' expr)))
    NixInheritBinding {} -> Left (NotANormalBinding bindingIdx)

renameAttrPathKeyInPath :: Int -> Int -> LAttrKey -> LAttrPath -> EditM LAttrPath
renameAttrPathKeyInPath _ keyIdx replacementKey (L l (NixAttrPath ann keys))
  | otherwise = do
      oldKey <- elementAt keyIdx keys
      let replacement' = translateFromTo (getLoc replacementKey) (getLoc oldKey) replacementKey
      Right (normalizeAttrPathLayout (L l (NixAttrPath ann (replaceAt keyIdx replacement' keys))))

--------------------------------------------------------------------------------

-- Small collection/index helpers

normalizeInsertIndex :: BindingInsertPosition -> Int -> EditM Int
normalizeInsertIndex AppendBinding len = Right len
normalizeInsertIndex (InsertBindingAt idx) len
  | idx < 0 = Left (NegativeIndex idx)
  | idx > len = Left (IndexOutOfRange idx len)
  | otherwise = Right idx

normalizeListInsertIndex :: ListInsertPosition -> Int -> EditM Int
normalizeListInsertIndex AppendListElement len = Right len
normalizeListInsertIndex (InsertListElementAt idx) len
  | idx < 0 = Left (NegativeIndex idx)
  | idx > len = Left (IndexOutOfRange idx len)
  | otherwise = Right idx

deleteAt :: Int -> [a] -> EditM [a]
deleteAt idx xs
  | otherwise = elementAt idx xs >> Right (take idx xs <> drop (idx + 1) xs)

elementAt :: Int -> [a] -> EditM a
elementAt idx xs
  | idx < 0 = Left (NegativeIndex idx)
  | idx >= length xs = Left (IndexOutOfRange idx (length xs))
  | otherwise = Right (xs !! idx)

insertAt :: Int -> a -> [a] -> [a]
insertAt idx x xs = take idx xs <> [x] <> drop idx xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx x xs = take idx xs <> [x] <> drop (idx + 1) xs

--------------------------------------------------------------------------------

-- Container rebuild style and anchoring

data SetLayoutStyle = SetInline | SetMultiline

data ListLayoutStyle = ListInline | ListMultiline

data BodyLayoutStyle = BodyInline | BodyMultiline

data BindingSequenceAnchor = AnchorPreserveExisting | AnchorStartAtFirstSlot

--------------------------------------------------------------------------------

-- Set rebuild primitives

-- | Internal primitive: rebuild a set container from its current bindings.
rebuildSetLayout :: AnnSet -> NixSetIsRecursive -> [LBinding] -> EditM Expr
rebuildSetLayout ann kind bindings = rebuildSetLayoutWithAnchor AnchorPreserveExisting ann kind bindings

rebuildSetLayoutWithAnchor :: BindingSequenceAnchor -> AnnSet -> NixSetIsRecursive -> [LBinding] -> EditM Expr
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
    ann' = prepareSetLayout annWithClose shiftedBindings
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

--------------------------------------------------------------------------------

-- Let rebuild primitives

-- | Internal primitive: rebuild a let container from bindings and body.
rebuildLetLayout :: AnnLetNode -> [LBinding] -> LExpr -> EditM Expr
rebuildLetLayout ann bindings body =
  rebuildLetLayoutWithAnchor AnchorPreserveExisting ann bindings body

rebuildLetLayoutWithAnchor :: BindingSequenceAnchor -> AnnLetNode -> [LBinding] -> LExpr -> EditM Expr
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
    finalAnn = setAnnCommon annCommon' preparedAnn
    bindingsSpan = listSpanOr (letSpan `combineSrcSpans` newInSpan) shiftedBindings

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

letInCursor :: SetLayoutStyle -> SrcSpan -> [LBinding] -> RenderCursor
letInCursor style oldIn bindings =
  case (style, reverse bindings) of
    (_, []) -> cursorAtSpanStart oldIn
    (SetInline, lastBinding : _) ->
      advanceCursor (cursorAtSpanStart (getLoc lastBinding)) (renderBindingSyntax (unLoc lastBinding) <> " ")
    (SetMultiline, lastBinding : _) ->
      let endCursor = advanceCursor (cursorAtSpanStart (getLoc lastBinding)) (renderBindingSyntax (unLoc lastBinding))
       in RenderCursor (rcLine endCursor + 1) (srcSpanStartColumn oldIn)

inferBodyLayoutStyle :: SrcSpan -> SrcSpan -> BodyLayoutStyle
inferBodyLayoutStyle anchor bodySpan
  | srcSpanStartLine bodySpan == srcSpanEndLine anchor = BodyInline
  | otherwise = BodyMultiline

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

--------------------------------------------------------------------------------

-- List rebuild primitives

-- | Internal primitive: rebuild a list container from its current elements.
rebuildListLayout :: AnnListNode -> [LExpr] -> EditM Expr
rebuildListLayout ann xs =
  rebuildListLayoutWithAnchor AnchorPreserveExisting ann xs

rebuildListLayoutWithAnchor :: BindingSequenceAnchor -> AnnListNode -> [LExpr] -> EditM Expr
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

rebuildListElements :: String -> ListLayoutStyle -> RenderCursor -> [LExpr] -> [LExpr]
rebuildListElements targetFile style startCursor = snd . foldl step (startCursor, [])
  where
    step (cursor, acc) expr =
      let shifted = normalizeExprLayout (reanchorExpr targetFile cursor expr)
          next = nextListElementCursor style cursor shifted
       in (next, acc <> [shifted])

nextListElementCursor :: ListLayoutStyle -> RenderCursor -> LExpr -> RenderCursor
nextListElementCursor style startCursor expr =
  case style of
    ListInline -> advanceCursor endCursor " "
    ListMultiline -> RenderCursor (rcLine endCursor + 1) (rcColumn startCursor)
  where
    endCursor = advanceCursor startCursor (renderExprSyntax (unLoc expr))

listCloseCursor :: ListLayoutStyle -> SrcSpan -> [LExpr] -> RenderCursor
listCloseCursor style oldClose xs =
  case (style, reverse xs) of
    (_, []) -> cursorAtSpanStart oldClose
    (ListInline, lastElem : _) ->
      advanceCursor (cursorAtSpanStart (getLoc lastElem)) (renderExprSyntax (unLoc lastElem) <> " ")
    (ListMultiline, lastElem : _) ->
      let endCursor = advanceCursor (cursorAtSpanStart (getLoc lastElem)) (renderExprSyntax (unLoc lastElem))
       in RenderCursor (rcLine endCursor + 1) (srcSpanStartColumn oldClose)

reanchorExpr :: String -> RenderCursor -> LExpr -> LExpr
reanchorExpr targetFile target expr@(L originalSpan _) =
  translateFromTo originalSpan targetSpan expr
  where
    targetSpan = mkSrcSpan targetFile (rcLine target, rcColumn target) (rcLine target, rcColumn target + 1)

--------------------------------------------------------------------------------

-- Shared cursor/span helpers used by rebuild logic

shiftComments :: SrcSpan -> SrcSpan -> [Located Comment] -> [Located Comment]
shiftComments oldSpan newSpan = fmap (translateFromTo oldSpan newSpan)

applyDeltaToAnchor :: SrcSpan -> DeltaPos -> SrcSpan
applyDeltaToAnchor anchor delta =
  case delta of
    DeltaPos 0 col ->
      mkSrcSpan
        (srcSpanFilename anchor)
        (srcSpanEndLine anchor, srcSpanEndColumn anchor + col)
        (srcSpanEndLine anchor, srcSpanEndColumn anchor + col + 1)
    DeltaPos line col ->
      mkSrcSpan
        (srcSpanFilename anchor)
        (srcSpanEndLine anchor + line, col + 1)
        (srcSpanEndLine anchor + line, col + 2)

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

inlineInsertCursor :: SrcSpan -> SrcSpan -> RenderCursor
inlineInsertCursor openSpan closeSpan =
  let inlineGap = max 1 (srcSpanStartColumn closeSpan - srcSpanEndColumn openSpan)
   in RenderCursor (srcSpanEndLine openSpan) (srcSpanEndColumn openSpan + inlineGap)

preservesExistingAnchors :: BindingSequenceAnchor -> Bool
preservesExistingAnchors = \case
  AnchorPreserveExisting -> True
  AnchorStartAtFirstSlot -> False

--------------------------------------------------------------------------------

-- Shared binding/list rebuild kernels

rebuildBindingsLayout :: String -> SetLayoutStyle -> RenderCursor -> [LBinding] -> [LBinding]
rebuildBindingsLayout targetFile style startCursor = snd . foldl step (startCursor, [])
  where
    step (cursor, acc) binding =
      let shifted = normalizeBindingLayout (reanchorBinding targetFile cursor binding)
          next = nextBindingCursor style cursor shifted
       in (next, acc <> [shifted])

nextBindingCursor :: SetLayoutStyle -> RenderCursor -> LBinding -> RenderCursor
nextBindingCursor style startCursor binding =
  case style of
    SetInline -> advanceCursor endCursor " "
    SetMultiline -> RenderCursor (rcLine endCursor + 1) (rcColumn startCursor)
  where
    endCursor = advanceCursor startCursor (renderBindingSyntax (unLoc binding))

closeCursor :: SetLayoutStyle -> SrcSpan -> [LBinding] -> RenderCursor
closeCursor style oldClose bindings =
  case (style, reverse bindings) of
    (_, []) -> cursorAtSpanStart oldClose
    (SetInline, lastBinding : _) ->
      advanceCursor (cursorAtSpanStart (getLoc lastBinding)) (renderBindingSyntax (unLoc lastBinding) <> " ")
    (SetMultiline, lastBinding : _) ->
      let endCursor = advanceCursor (cursorAtSpanStart (getLoc lastBinding)) (renderBindingSyntax (unLoc lastBinding))
       in RenderCursor (rcLine endCursor + 1) (srcSpanStartColumn oldClose)

reanchorBinding :: String -> RenderCursor -> LBinding -> LBinding
reanchorBinding targetFile target binding@(L originalSpan _) =
  translateFromTo originalSpan targetSpan binding
  where
    targetSpan = mkSrcSpan targetFile (rcLine target, rcColumn target) (rcLine target, rcColumn target + 1)

tokenSpanAt :: RenderCursor -> AnnToken -> SrcSpan
tokenSpanAt cursor tok =
  mkSrcSpan fileName (rcLine cursor, rcColumn cursor) (rcLine cursor, rcColumn cursor + tokenWidth tok)
  where
    fileName = maybe "<edited>" srcSpanFilename (annTokenSrcSpan tok)

--------------------------------------------------------------------------------

tokenWidth :: AnnToken -> Int
tokenWidth tok = max 1 . T.length $ fromMaybe " " (showToken (annToken tok))

renderBindingSyntax :: Binding -> Text
renderBindingSyntax = renderStrict . layoutPretty defaultLayoutOptions . output

renderExprSyntax :: Expr -> Text
renderExprSyntax = renderStrict . layoutPretty defaultLayoutOptions . output

--------------------------------------------------------------------------------

-- Layout normalization

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
          Just semiSpan -> mapTokenToDelta (deltaFromAnchor (exprSpan (unLoc expr')) semiSpan) (anbSemicolon ann)
          Nothing -> anbSemicolon ann
     in NixNormalBinding ann {anbEqual = equalTok, anbSemicolon = semTok} path' expr'
  other -> other

--------------------------------------------------------------------------------

normalizeAttrPathLayout :: LAttrPath -> LAttrPath
normalizeAttrPathLayout (L l (NixAttrPath ann keys)) = L l $ NixAttrPath ann' keys
  where
    ann' = ann {aapDots = normalizeDots keys (aapDots ann)}
    normalizeDots pathKeys dots = zipWith mkDot pathKeys (take (max 0 (length pathKeys - 1)) (dots <> repeat fallbackDot))
    mkDot key tok = case annTokenSrcSpan tok of
      Just dotSpan -> mapTokenToDelta (deltaFromAnchor (getLoc key) dotSpan) tok
      Nothing -> deltaAnnToken AnnDot (DeltaPos 0 0)
    fallbackDot = deltaAnnToken AnnDot (DeltaPos 0 0)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- Recursive repair walkers

repairLocatedExpr :: LExpr -> EditM LExpr
repairLocatedExpr (L originalSpan node) =
  case node of
    NixVar ann ident -> Right (repairVarExpr originalSpan ann ident)
    NixLit ann lit -> Right (repairLitExpr originalSpan ann lit)
    NixPar ann inner -> repairParExpr originalSpan ann inner
    NixString ann str -> Right (repairStringExpr originalSpan ann str)
    NixPath ann path -> Right (repairPathExpr originalSpan ann path)
    NixEnvPath ann path -> Right (repairEnvPathExpr originalSpan ann path)
    NixLam ann pat body -> repairLambdaExpr originalSpan ann pat body
    NixApp ann lhs rhs -> repairAppExpr originalSpan ann lhs rhs
    NixBinApp ann op lhs rhs -> repairBinAppExpr originalSpan ann op lhs rhs
    NixNotApp ann inner -> repairPrefixExpr originalSpan ann NixNotApp inner
    NixNegApp ann inner -> repairPrefixExpr originalSpan ann NixNegApp inner
    NixList ann xs -> repairListExpr ann xs
    NixSet ann kind (L _ bindings) -> repairSetExpr ann kind bindings
    NixLet ann (L _ bindings) body -> repairLetExpr ann bindings body
    NixHasAttr ann lhs path -> repairHasAttrExpr originalSpan ann lhs path
    NixSelect ann lhs path def -> repairSelectExpr originalSpan ann lhs path def
    NixIf ann cond thenExpr elseExpr -> repairIfExpr originalSpan ann cond thenExpr elseExpr
    NixWith ann scope body -> repairWithExpr originalSpan ann scope body
    NixAssert ann assertion body -> repairAssertExpr originalSpan ann assertion body

repairLocatedExprAt :: RenderCursor -> LExpr -> EditM LExpr
repairLocatedExprAt cursor expr = repairLocatedExpr (translateFromTo (getLoc expr) (cursorSpan cursor (srcSpanFilename (getLoc expr))) expr)

repairVarExpr :: SrcSpan -> AnnCommon -> LId -> LExpr
repairVarExpr originalSpan ann ident =
  let ident' = repairLocatedIdAt (spanStartCursor originalSpan) ident
      span' = getLoc ident'
      ann' = setAnnSpan span' ann
   in L span' (NixVar ann' ident')

repairLitExpr :: SrcSpan -> AnnCommon -> LLit -> LExpr
repairLitExpr originalSpan ann lit =
  let lit' = repairLocatedLitAt (spanStartCursor originalSpan) lit
      span' = getLoc lit'
      ann' = setAnnSpan span' ann
   in L span' (NixLit ann' lit')

repairStringExpr :: SrcSpan -> AnnStringNode -> LNString -> LExpr
repairStringExpr originalSpan ann str =
  let str' = repairLocatedStringAt (spanStartCursor originalSpan) str
      span' = getLoc str'
      ann' = setAnnSpan span' ann
   in L span' (NixString ann' str')

repairPathExpr :: SrcSpan -> AnnPathNode -> LPath -> LExpr
repairPathExpr originalSpan ann path =
  let path' = repairLocatedPathAt (spanStartCursor originalSpan) path
      span' = getLoc path'
      ann' = setAnnSpan span' ann
   in L span' (NixPath ann' path')

repairParExpr :: SrcSpan -> AnnParNode -> LExpr -> EditM LExpr
repairParExpr originalSpan ann inner = do
  let openSpan = tokenSpanAt (spanStartCursor originalSpan) (apnOpenP ann)
  inner' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "paren open" (apnOpenP ann)) (getLoc inner) openSpan) inner
  let closeSpan = preserveGapSpan (getLoc inner) (expectTokenSpan "paren close" (apnCloseP ann)) inner'
      ann0 = ann {apnOpenP = (apnOpenP ann) {annTokenPos = AnnSpan openSpan}, apnCloseP = (apnCloseP ann) {annTokenPos = AnnSpan closeSpan}}
      ann' = setAnnSpan (openSpan `combineSrcSpans` closeSpan) (prepareParLayout ann0 (unLoc inner'))
      expr' = NixPar ann' inner'
   in Right (L (exprSpan expr') expr')

repairEnvPathExpr :: SrcSpan -> AnnEnvPathNode -> Located Text -> LExpr
repairEnvPathExpr _ ann path =
  let openSpan = expectTokenSpan "env path open" (aenvOpen ann)
      path' = repairLocatedTextAt (spanStartCursor (preserveGapSpan openSpan (getLoc path) (L openSpan ()))) path
      closeSpan = preserveGapSpan (getLoc path) (expectTokenSpan "env path close" (aenvClose ann)) path'
      ann' = setAnnSpan (openSpan `combineSrcSpans` closeSpan) ann {aenvClose = (aenvClose ann) {annTokenPos = AnnSpan closeSpan}}
      span' = openSpan `combineSrcSpans` closeSpan
   in L span' (NixEnvPath ann' path')

repairLambdaExpr :: SrcSpan -> AnnLamNode -> LFuncPat -> LExpr -> EditM LExpr
repairLambdaExpr originalSpan ann pat body = do
  pat' <- repairLocatedFuncPatAt (spanStartCursor originalSpan) pat
  let colonSpan = preserveGapSpan (funcPatRenderSpan (unLoc pat)) (expectTokenSpan "lambda colon" (alamColon ann)) pat'
  body' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "lambda colon" (alamColon ann)) (getLoc body) colonSpan) body
  let span' = funcPatRenderSpan (unLoc pat') `combineSrcSpans` getLoc body'
      ann' = setAnnSpan span' ann {alamColon = (alamColon ann) {annTokenPos = AnnSpan colonSpan}}
   in Right (L span' (NixLam ann' pat' body'))

repairAppExpr :: SrcSpan -> AnnAppNode -> LExpr -> LExpr -> EditM LExpr
repairAppExpr originalSpan ann lhs rhs = do
  lhs' <- repairLocatedExprAt (spanStartCursor originalSpan) lhs
  rhs' <- repairLocatedExprAt (preserveGapTarget (getLoc lhs) (getLoc rhs) (getLoc lhs')) rhs
  let span' = getLoc lhs' `combineSrcSpans` getLoc rhs'
      ann' = setAnnSpan span' ann
   in Right (L span' (NixApp ann' lhs' rhs'))

repairBinAppExpr :: SrcSpan -> AnnBinAppNode -> BinaryOp -> LExpr -> LExpr -> EditM LExpr
repairBinAppExpr originalSpan ann op lhs rhs = do
  lhs' <- repairLocatedExprAt (spanStartCursor originalSpan) lhs
  let opSpan = preserveGapSpan (getLoc lhs) (expectTokenSpan "binary operator" (abinOperator ann)) lhs'
  rhs' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "binary operator" (abinOperator ann)) (getLoc rhs) opSpan) rhs
  let span' = getLoc lhs' `combineSrcSpans` getLoc rhs'
      ann' = setAnnSpan span' ann {abinOperator = (abinOperator ann) {annTokenPos = AnnSpan opSpan}}
   in Right (L span' (NixBinApp ann' op lhs' rhs'))

repairPrefixExpr :: SrcSpan -> AnnPrefixNode -> (AnnPrefixNode -> LExpr -> Expr) -> LExpr -> EditM LExpr
repairPrefixExpr originalSpan ann mkNode inner = do
  let tokSpan = tokenSpanAt (spanStartCursor originalSpan) (apfxToken ann)
  inner' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "prefix operator" (apfxToken ann)) (getLoc inner) tokSpan) inner
  let span' = tokSpan `combineSrcSpans` getLoc inner'
      ann' = setAnnSpan span' ann {apfxToken = (apfxToken ann) {annTokenPos = AnnSpan tokSpan}}
   in Right (L span' (mkNode ann' inner'))

repairListExpr :: AnnListNode -> [LExpr] -> EditM LExpr
repairListExpr ann xs = do
  xs' <- mapM repairLocatedExpr xs
  repaired <- rebuildListLayout ann xs'
  Right (L (exprSpan repaired) repaired)

repairSetExpr :: AnnSet -> NixSetIsRecursive -> [LBinding] -> EditM LExpr
repairSetExpr ann kind bindings = do
  bindings' <- mapM repairLocatedBinding bindings
  repaired <- rebuildSetLayout ann kind bindings'
  Right (L (exprSpan repaired) repaired)

repairLetExpr :: AnnLetNode -> [LBinding] -> LExpr -> EditM LExpr
repairLetExpr ann bindings body = do
  bindings' <- mapM repairLocatedBinding bindings
  body' <- repairLocatedExpr body
  repaired <- rebuildLetLayout ann bindings' body'
  Right (L (exprSpan repaired) repaired)

repairHasAttrExpr :: SrcSpan -> AnnHasAttr -> LExpr -> LAttrPath -> EditM LExpr
repairHasAttrExpr originalSpan ann lhs path = do
  lhs' <- repairLocatedExprAt (spanStartCursor originalSpan) lhs
  let qSpan = preserveGapSpan (getLoc lhs) (expectTokenSpan "has-attr question" (ahaQuestion ann)) lhs'
  path' <- repairLocatedAttrPathAt (preserveGapTarget (expectTokenSpan "has-attr question" (ahaQuestion ann)) (getLoc path) qSpan) path
  let span' = getLoc lhs' `combineSrcSpans` attrPathSpan (unLoc path')
      ann' = setAnnSpan span' (prepareHasAttrLayout (ann {ahaQuestion = (ahaQuestion ann) {annTokenPos = AnnSpan qSpan}}) (unLoc lhs') (unLoc path'))
   in Right (L span' (NixHasAttr ann' lhs' path'))

repairSelectExpr :: SrcSpan -> AnnSelect -> LExpr -> LAttrPath -> Maybe LExpr -> EditM LExpr
repairSelectExpr originalSpan ann lhs path def = do
  lhs' <- repairLocatedExprAt (spanStartCursor originalSpan) lhs
  path' <- repairLocatedAttrPathAt (preserveGapTarget (getLoc lhs) (getLoc path) (getLoc lhs')) path
  def' <- traverse (repairLocatedExprAtSelect ann path path') def
  let annWithOr = case (aslOr ann, def, def') of
        (Just orTok, Just _, Just _) ->
          let orSpan = preserveGapSpan (attrPathRenderSpan (unLoc path)) (expectTokenSpan "select or" orTok) path'
           in ann {aslOr = Just (orTok {annTokenPos = AnnSpan orSpan})}
        _ -> ann
      ann' = setAnnSpan finalSpan (prepareSelectLayout annWithOr (unLoc lhs') (unLoc path') def')
      finalSpan = maybe baseSpan (combineSrcSpans baseSpan . getLoc) def'
      baseSpan = getLoc lhs' `combineSrcSpans` attrPathSpan (unLoc path')
   in Right (L finalSpan (NixSelect ann' lhs' path' def'))

repairIfExpr :: SrcSpan -> AnnIfNode -> LExpr -> LExpr -> LExpr -> EditM LExpr
repairIfExpr originalSpan ann cond thenExpr elseExpr = do
  let ifSpan = tokenSpanAt (spanStartCursor originalSpan) (aifIf ann)
  cond' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "if keyword" (aifIf ann)) (getLoc cond) ifSpan) cond
  let thenSpan = preserveGapSpan (getLoc cond) (expectTokenSpan "then keyword" (aifThen ann)) cond'
  then' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "then keyword" (aifThen ann)) (getLoc thenExpr) thenSpan) thenExpr
  let elseSpan = preserveGapSpan (getLoc thenExpr) (expectTokenSpan "else keyword" (aifElse ann)) then'
  else' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "else keyword" (aifElse ann)) (getLoc elseExpr) elseSpan) elseExpr
  let ann0 = ann {aifIf = (aifIf ann) {annTokenPos = AnnSpan ifSpan}, aifThen = (aifThen ann) {annTokenPos = AnnSpan thenSpan}, aifElse = (aifElse ann) {annTokenPos = AnnSpan elseSpan}}
      span' = ifSpan `combineSrcSpans` getLoc else'
      ann' = setAnnSpan span' (prepareIfLayout ann0 (unLoc cond') (unLoc then') (unLoc else'))
   in Right (L span' (NixIf ann' cond' then' else'))

repairWithExpr :: SrcSpan -> AnnWithNode -> LExpr -> LExpr -> EditM LExpr
repairWithExpr originalSpan ann scope body = do
  let withSpan = tokenSpanAt (spanStartCursor originalSpan) (awWith ann)
  scope' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "with keyword" (awWith ann)) (getLoc scope) withSpan) scope
  let semiSpan = preserveGapSpan (getLoc scope) (expectTokenSpan "with semicolon" (awSemicolon ann)) scope'
  body' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "with semicolon" (awSemicolon ann)) (getLoc body) semiSpan) body
  let ann0 = ann {awWith = (awWith ann) {annTokenPos = AnnSpan withSpan}, awSemicolon = (awSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
      span' = withSpan `combineSrcSpans` getLoc body'
      ann' = setAnnSpan span' (prepareWithLayout ann0 (unLoc scope') (unLoc body'))
   in Right (L span' (NixWith ann' scope' body'))

repairAssertExpr :: SrcSpan -> AnnAssertNode -> LExpr -> LExpr -> EditM LExpr
repairAssertExpr originalSpan ann assertion body = do
  let assertSpan = tokenSpanAt (spanStartCursor originalSpan) (aaAssert ann)
  assertion' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "assert keyword" (aaAssert ann)) (getLoc assertion) assertSpan) assertion
  let semiSpan = preserveGapSpan (getLoc assertion) (expectTokenSpan "assert semicolon" (aaSemicolon ann)) assertion'
  body' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "assert semicolon" (aaSemicolon ann)) (getLoc body) semiSpan) body
  let ann0 = ann {aaAssert = (aaAssert ann) {annTokenPos = AnnSpan assertSpan}, aaSemicolon = (aaSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
      span' = assertSpan `combineSrcSpans` getLoc body'
      ann' = setAnnSpan span' (prepareAssertLayout ann0 (unLoc assertion') (unLoc body'))
   in Right (L span' (NixAssert ann' assertion' body'))

--------------------------------------------------------------------------------

-- Binding, attr-path, and function-pattern repair walkers

repairLocatedBinding :: LBinding -> EditM LBinding
repairLocatedBinding (L originalSpan node) =
  case node of
    NixNormalBinding ann path expr -> repairLocatedNormalBinding originalSpan ann path expr
    NixInheritBinding ann mScope names -> repairLocatedInheritBinding originalSpan ann mScope names

repairLocatedNormalBinding :: SrcSpan -> AnnNormalBinding -> LAttrPath -> LExpr -> EditM LBinding
repairLocatedNormalBinding originalSpan ann path expr = do
  path' <- repairLocatedAttrPathAt (spanStartCursor originalSpan) path
  let eqSpan = preserveGapSpan (attrPathSpan (unLoc path)) (expectTokenSpan "binding equals" (anbEqual ann)) path'
  expr' <- repairLocatedExprAt (preserveGapTarget (expectTokenSpan "binding equals" (anbEqual ann)) (getLoc expr) eqSpan) expr
  let semiSpan = preserveGapSpan (getLoc expr) (expectTokenSpan "binding semicolon" (anbSemicolon ann)) expr'
      ann' = setAnnSpan span' ann {anbEqual = (anbEqual ann) {annTokenPos = AnnSpan eqSpan}, anbSemicolon = (anbSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
      span' = attrPathSpan (unLoc path') `combineSrcSpans` semiSpan
   in Right (L span' (NixNormalBinding ann' path' expr'))

repairLocatedInheritBinding :: SrcSpan -> AnnInheritBinding -> Maybe LExpr -> [LAttrKey] -> EditM LBinding
repairLocatedInheritBinding originalSpan ann mScope names = do
  let inheritSpan = tokenSpanAt (spanStartCursor originalSpan) (aibInherit ann)
  scope' <- case mScope of
    Nothing -> Right Nothing
    Just scopeExpr ->
      Just
        <$> repairLocatedExprAt
          (preserveGapTarget (expectTokenSpan "inherit keyword" (aibInherit ann)) (getLoc scopeExpr) inheritSpan)
          scopeExpr
  names' <- repairInheritNames inheritSpan mScope scope' names
  let anchorOld = maybe (maybe (expectTokenSpan "inherit keyword" (aibInherit ann)) getLoc (lastMay names)) getLoc mScope
      anchorNew = maybe (maybe inheritSpan getLoc (lastMay names')) getLoc scope'
      semiSpan = preserveGapSpan anchorOld (expectTokenSpan "inherit semicolon" (aibSemicolon ann)) (L anchorNew ())
      span' = foldr combineSrcSpans (inheritSpan `combineSrcSpans` semiSpan) (maybe [] ((: []) . getLoc) scope' <> fmap getLoc names')
      ann' = setAnnSpan span' ann {aibInherit = (aibInherit ann) {annTokenPos = AnnSpan inheritSpan}, aibSemicolon = (aibSemicolon ann) {annTokenPos = AnnSpan semiSpan}}
   in Right (L span' (NixInheritBinding ann' scope' names'))

--------------------------------------------------------------------------------

repairLocatedAttrPath :: LAttrPath -> EditM LAttrPath
repairLocatedAttrPath path = repairLocatedAttrPathAt (spanStartCursor (getLoc path)) path

repairLocatedAttrPathAt :: RenderCursor -> LAttrPath -> EditM LAttrPath
repairLocatedAttrPathAt cursor (L _ (NixAttrPath ann keys)) =
  case keys of
    [] -> Left EmptyAttrPathEdit
    firstKey : restKeys -> repairAttrPathHead cursor ann keys firstKey restKeys

repairAttrPathHead :: RenderCursor -> AnnAttrPath -> [LAttrKey] -> LAttrKey -> [LAttrKey] -> EditM LAttrPath
repairAttrPathHead cursor ann keys firstKey restKeys =
  case (leadingDot, dotTokens) of
    (True, firstDot : restDots) -> do
      let firstDotSpan = tokenSpanAt cursor firstDot
      firstKey' <- repairLocatedAttrKeyAt (preserveGapTarget (expectTokenSpan "attr path dot" firstDot) (getLoc firstKey) firstDotSpan) firstKey
      (dots', keys') <- repairAttrTail restDots restKeys (getLoc firstKey) firstKey'
      let ann' = setAnnSpan span' ann {aapDots = firstDot {annTokenPos = AnnSpan firstDotSpan} : dots'}
          span' = firstDotSpan `combineSrcSpans` foldr1 combineSrcSpans (getLoc firstKey' : fmap getLoc keys')
       in Right (L span' (NixAttrPath ann' (firstKey' : keys')))
    _ -> do
      firstKey' <- repairLocatedAttrKeyAt cursor firstKey
      (dots', keys') <- repairAttrTail dotTokens restKeys (getLoc firstKey) firstKey'
      let span' = foldr1 combineSrcSpans (getLoc firstKey' : fmap getLoc keys')
          ann' = setAnnSpan span' ann {aapDots = dots'}
       in Right (L span' (NixAttrPath ann' (firstKey' : keys')))
  where
    leadingDot = length (aapDots ann) == length keys && not (null (aapDots ann))
    dotTokens = aapDots ann

--------------------------------------------------------------------------------

repairLocatedFuncPat :: LFuncPat -> EditM LFuncPat
repairLocatedFuncPat pat = repairLocatedFuncPatAt (spanStartCursor (getLoc pat)) pat

repairLocatedFuncPatAt :: RenderCursor -> LFuncPat -> EditM LFuncPat
repairLocatedFuncPatAt cursor (L _ node) =
  case node of
    NixVarPat ann ident ->
      let ident' = repairLocatedIdAt cursor ident
          span' = getLoc ident'
          ann' = setAnnSpan span' ann {avpId = span'}
       in Right (L span' (NixVarPat ann' ident'))
    NixSetPat ann ellipses mAs bindings -> repairLocatedSetPat cursor ann ellipses mAs bindings

repairLocatedSetPat :: RenderCursor -> AnnSetPatNode -> NixSetPatEllipses -> Maybe LSetPatAs -> [LSetPatBinding] -> EditM LFuncPat
repairLocatedSetPat cursor ann ellipses mAs bindings = do
  let openSpan = tokenSpanAt cursor (aspOpenC ann)
  mAs' <- traverse (repairSetPatAsAt cursor) mAs
  let entryCursor = setPatEntryCursor openSpan mAs'
  bindings' <- repairSetPatBindingsAt ann entryCursor bindings
  let closeSpan = repairSetPatCloseSpan ann bindings' ellipses
      ann' = setAnnSpan patSpan ann {aspOpenC = (aspOpenC ann) {annTokenPos = AnnSpan openSpan}, aspCloseC = (aspCloseC ann) {annTokenPos = AnnSpan closeSpan}}
      patNode = NixSetPat ann' ellipses mAs' bindings'
      patSpan = funcPatBodySpan patNode
   in Right (L patSpan patNode)

setPatEntryCursor :: SrcSpan -> Maybe LSetPatAs -> RenderCursor
setPatEntryCursor openSpan = \case
  Just asPat@(L _ NixSetPatAs {nspaLocation = NixSetPatAsLeading}) -> spanStartCursor (getLoc asPat)
  _ -> RenderCursor (srcSpanStartLine openSpan) (srcSpanEndColumn openSpan)

repairLocatedAttrKeyAt :: RenderCursor -> LAttrKey -> EditM LAttrKey
repairLocatedAttrKeyAt cursor (L _ key) =
  let span' = case key of
        NixStaticAttrKey _ ident -> textSpanAt (cursorFile ident) cursor (unLoc ident)
        NixDynamicStringAttrKey _ _ -> textSpanAt cursorFallbackFile cursor (renderAttrKeyTextLocal key)
        NixDynamicInterpolAttrKey _ _ -> textSpanAt cursorFallbackFile cursor (renderAttrKeyTextLocal key)
      key' = case key of
        NixStaticAttrKey ann ident -> NixStaticAttrKey ann (repairLocatedIdAt cursor ident)
        other -> other
   in Right (L span' key')

--------------------------------------------------------------------------------

-- Leaf repair helpers

repairLocatedLitAt :: RenderCursor -> LLit -> LLit
repairLocatedLitAt cursor (L _ lit) = L (textSpanAt cursorFallbackFile cursor (renderLitTextLocal lit)) lit

--------------------------------------------------------------------------------

repairLocatedStringAt :: RenderCursor -> LNString -> LNString
repairLocatedStringAt cursor (L _ str) = L (textSpanAt cursorFallbackFile cursor (renderStringTextLocal str)) str

--------------------------------------------------------------------------------

repairLocatedPathAt :: RenderCursor -> LPath -> LPath
repairLocatedPathAt cursor (L _ path) = L (textSpanAt cursorFallbackFile cursor (renderPathTextLocal path)) path

--------------------------------------------------------------------------------

repairLocatedTextAt :: RenderCursor -> Located Text -> Located Text
repairLocatedTextAt cursor (L _ txt) = L (textSpanAt cursorFallbackFile cursor txt) txt

--------------------------------------------------------------------------------

repairLocatedIdAt :: RenderCursor -> LId -> LId
repairLocatedIdAt cursor (L _ ident) = L (textSpanAt cursorFallbackFile cursor ident) ident

--------------------------------------------------------------------------------

repairSetPatAsAt :: RenderCursor -> LSetPatAs -> EditM LSetPatAs
repairSetPatAsAt cursor (L _ asPat@NixSetPatAs {..}) =
  let var' = repairLocatedIdAt cursor nspaVar
      atSpan = case nspaLocation of
        NixSetPatAsLeading -> tokenSpanAt (spanEndCursor (getLoc var')) (aspaAt nspaAnn)
        NixSetPatAsTrailing -> tokenSpanAt cursor (aspaAt nspaAnn)
      span' = setPatAsRenderSpan (nspaAnn {aspaAt = (aspaAt nspaAnn) {annTokenPos = AnnSpan atSpan}}) var' nspaLocation
      ann' = setAnnSpan span' nspaAnn {aspaAt = (aspaAt nspaAnn) {annTokenPos = AnnSpan atSpan}}
   in Right (L span' asPat {nspaAnn = ann', nspaVar = var'})

repairSetPatBindingsAt :: AnnSetPatNode -> RenderCursor -> [LSetPatBinding] -> EditM [LSetPatBinding]
repairSetPatBindingsAt ann startCursor bindings = snd <$> foldl step (Right (startCursor, [])) (zip [0 ..] bindings)
  where
    step acc (idx, binding) = do
      (cursor, repaired) <- acc
      binding' <- repairSetPatBindingAt ann idx cursor binding
      let next = advanceCursor (spanStartCursor (getLoc binding')) (renderSetPatBindingSyntax (unLoc binding'))
      pure (next, repaired <> [binding'])

repairSetPatBindingAt :: AnnSetPatNode -> Int -> RenderCursor -> LSetPatBinding -> EditM LSetPatBinding
repairSetPatBindingAt _ _ cursor (L _ binding@NixSetPatBinding {..}) = do
  let var' = repairLocatedIdAt cursor nspbVar
  def' <- case nspbDefault of
    Nothing -> Right Nothing
    Just defExpr ->
      Just
        <$> repairLocatedExprAt
          (preserveGapTargetForSetPat binding var' defExpr)
          defExpr
  let qTok' = case (aspbQuestion nspbAnn, nspbDefault, def') of
        (Just qTok, Just _, Just _) ->
          let qSpan = preserveGapSpan (getLoc nspbVar) (expectTokenSpan "set pattern question" qTok) var'
           in Just (qTok {annTokenPos = AnnSpan qSpan})
        _ -> aspbQuestion nspbAnn
      binding' = NixSetPatBinding (setAnnSpan span' nspbAnn {aspbQuestion = qTok'}) var' def'
      span' = maybe (getLoc var') (combineSrcSpans (getLoc var') . getLoc) def'
   in Right (L span' binding')

--------------------------------------------------------------------------------

repairAttrTail :: [AnnToken] -> [LAttrKey] -> SrcSpan -> LAttrKey -> EditM ([AnnToken], [LAttrKey])
repairAttrTail [] [] _ _ = Right ([], [])
repairAttrTail (dotTok : dotToks) (key : keys) oldPrev newPrev = do
  let dotSpan = preserveGapSpan oldPrev (expectTokenSpan "attr path dot" dotTok) (L (getLoc newPrev) ())
  key' <- repairLocatedAttrKeyAt (preserveGapTarget dotSpan (getLoc key) dotSpan) key
  (dots', keys') <- repairAttrTail dotToks keys (getLoc key) key'
  pure (dotTok {annTokenPos = AnnSpan dotSpan} : dots', key' : keys')
repairAttrTail dots keys _ _ = Left (IndexOutOfRange (length dots) (length keys))

--------------------------------------------------------------------------------

repairInheritNames :: SrcSpan -> Maybe (LExpr) -> Maybe (LExpr) -> [LAttrKey] -> EditM [LAttrKey]
repairInheritNames inheritSpan oldScope newScope names = snd <$> foldl step (Right (startAnchor, [])) names
  where
    startAnchor = maybe inheritSpan getLoc newScope
    oldStartAnchor = maybe inheritSpan getLoc oldScope
    step acc name = do
      (prevNew, repaired) <- acc
      let oldPrev = if null repaired then oldStartAnchor else getLoc (last names)
      name' <- repairLocatedAttrKeyAt (preserveGapTarget oldPrev (getLoc name) prevNew) name
      pure (getLoc name', repaired <> [name'])

--------------------------------------------------------------------------------

repairLocatedExprAtSelect :: AnnSelect -> LAttrPath -> LAttrPath -> LExpr -> EditM LExpr
repairLocatedExprAtSelect ann oldPath newPath oldDef =
  case aslOr ann of
    Just orTok ->
      let orSpan = preserveGapSpan (attrPathRenderSpan (unLoc oldPath)) (expectTokenSpan "select or" orTok) newPath
       in repairLocatedExprAt (preserveGapTarget (expectTokenSpan "select or" orTok) (getLoc oldDef) orSpan) oldDef
    Nothing -> repairLocatedExpr oldDef

--------------------------------------------------------------------------------

repairSetPatCloseSpan :: AnnSetPatNode -> [LSetPatBinding] -> NixSetPatEllipses -> SrcSpan
repairSetPatCloseSpan ann bindings _ =
  case reverse bindings of
    lastBinding : _ ->
      let endCursor = advanceCursor (spanStartCursor (getLoc lastBinding)) (renderSetPatBindingSyntax (unLoc lastBinding))
       in tokenSpanAt endCursor (aspCloseC ann)
    [] -> expectTokenSpan "set pattern close" (aspCloseC ann)

--------------------------------------------------------------------------------

renderSetPatBindingSyntax :: SetPatBinding -> Text
renderSetPatBindingSyntax = renderStrict . layoutPretty defaultLayoutOptions . output

setAnnSpan :: (HasAnnCommon a) => SrcSpan -> a -> a
setAnnSpan span' ann = setAnnCommon ((getAnnCommon ann) {acPos = AnnSpan span'}) ann

preserveGapTarget :: SrcSpan -> SrcSpan -> SrcSpan -> RenderCursor
preserveGapTarget oldAnchor oldTarget newAnchor = spanStartCursor (applyDeltaToAnchor newAnchor (deltaFromAnchor oldAnchor oldTarget))

preserveGapTargetForSetPat :: SetPatBinding -> LId -> LExpr -> RenderCursor
preserveGapTargetForSetPat binding var' oldDef =
  case aspbQuestion (nspbAnn binding) of
    Just qTok -> preserveGapTarget (expectTokenSpan "set pattern question" qTok) (getLoc oldDef) (preserveGapSpan (getLoc (nspbVar binding)) (expectTokenSpan "set pattern question" qTok) var')
    Nothing -> spanStartCursor (getLoc oldDef)

preserveGapSpan :: SrcSpan -> SrcSpan -> Located a -> SrcSpan
preserveGapSpan oldAnchor oldTarget newAnchor = applyDeltaToAnchor (getLoc newAnchor) (deltaFromAnchor oldAnchor oldTarget)

spanStartCursor :: SrcSpan -> RenderCursor
spanStartCursor = cursorAtSpanStart

spanEndCursor :: SrcSpan -> RenderCursor
spanEndCursor span' = RenderCursor (srcSpanEndLine span') (srcSpanEndColumn span')

cursorSpan :: RenderCursor -> String -> SrcSpan
cursorSpan cursor file = mkSrcSpan file (rcLine cursor, rcColumn cursor) (rcLine cursor, rcColumn cursor + 1)

textSpanAt :: String -> RenderCursor -> Text -> SrcSpan
textSpanAt file cursor txt =
  let end = advanceCursor cursor txt
   in mkSrcSpan file (rcLine cursor, rcColumn cursor) (rcLine end, rcColumn end)

cursorFallbackFile :: String
cursorFallbackFile = "<edited>"

cursorFile :: Located a -> String
cursorFile locd = srcSpanFilename (getLoc locd)

--------------------------------------------------------------------------------

-- Shared rendering, spacing, and translation helpers

renderLitTextLocal :: Lit -> Text
renderLitTextLocal = \case
  NixUri _ uri -> uri
  NixInteger _ int -> T.pack (show int)
  NixFloat _ float -> T.pack (show float)
  NixBoolean _ True -> "true"
  NixBoolean _ False -> "false"
  NixNull _ -> "null"

renderStringTextLocal :: NString -> Text
renderStringTextLocal = \case
  NixDoubleQuotesString (SourceText src) _ -> "\"" <> src <> "\""
  NixDoubleSingleQuotesString (SourceText src) _ -> "''" <> src <> "''"

renderPathTextLocal :: Path -> Text
renderPathTextLocal = \case
  NixLiteralPath _ path -> path
  NixInterpolPath (SourceText src) _ -> src

renderAttrKeyTextLocal :: AttrKey -> Text
renderAttrKeyTextLocal = \case
  NixStaticAttrKey _ ident -> unLoc ident
  NixDynamicStringAttrKey (SourceText src) _ -> "\"" <> src <> "\""
  NixDynamicInterpolAttrKey (SourceText src) _ -> "${" <> src <> "}"

--------------------------------------------------------------------------------

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)

listSpan :: [Located a] -> SrcSpan
listSpan [] = mkSrcSpan "<edited>" (1, 1) (1, 1)
listSpan xs = foldr1 combineSrcSpans (getLoc <$> xs)

listSpanOr :: SrcSpan -> [Located a] -> SrcSpan
listSpanOr fallback [] = fallback
listSpanOr _ xs = listSpan xs

--------------------------------------------------------------------------------

translateFromTo :: (Data a) => SrcSpan -> SrcSpan -> a -> a
translateFromTo origin target = everywhere (mkT translate)
  where
    originLine = srcSpanStartLine origin
    originColumn = srcSpanStartColumn origin
    targetLine = srcSpanStartLine target
    targetColumn = srcSpanStartColumn target
    targetFile = srcSpanFilename target
    translate :: SrcSpan -> SrcSpan
    translate src =
      mkSrcSpan
        targetFile
        (shiftPoint (srcSpanStartLine src) (srcSpanStartColumn src))
        (shiftPoint (srcSpanEndLine src) (srcSpanEndColumn src))
    shiftPoint line column =
      ( targetLine + (line - originLine),
        targetColumn + (column - originColumn)
      )
