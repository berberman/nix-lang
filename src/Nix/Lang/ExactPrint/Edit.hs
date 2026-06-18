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
    parseExpr,
    parseBinding,
    parseAttrKey,
    bindingSpan,
    insertSetBinding,
    replaceSetBinding,
    insertBinding,
    replaceBinding,
    replaceBindingValueNodeAt,
    insertListElement,
    replaceListElement,
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

import Nix.Lang.Annotation
import Data.Text (Text)
import Nix.Lang.ExactPrint.Edit.Fragment
import Nix.Lang.ExactPrint.Edit.Geometry
import Nix.Lang.ExactPrint.Repair
import Nix.Lang.ExactPrint.Edit.Rebuild
import Nix.Lang.ExactPrint.Edit.Types
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils

--------------------------------------------------------------------------------

-- Public edit operations

-- | Insert a parsed binding node into a set expression.
insertSetBinding :: BindingInsertPosition -> LBinding -> Expr -> EditResult Expr
insertSetBinding position binding = \case
  NixSet ann kind (L _ bindings) -> insertBindingIntoSet position ann kind bindings binding
  _ -> Left NotASet

-- | Replace a set binding with a parsed binding node.
replaceSetBinding :: Int -> LBinding -> Expr -> EditResult Expr
replaceSetBinding idx replacement = \case
  NixSet ann kind (L _ bindings) -> replaceBindingInSet idx ann kind bindings replacement
  _ -> Left NotASet

-- | Insert a parsed binding node into a set or let binding container.
insertBinding :: BindingInsertPosition -> LBinding -> Expr -> EditResult Expr
insertBinding position binding expr =
  case expr of
    NixSet ann kind (L _ bindings) -> insertBindingIntoSet position ann kind bindings binding
    NixLet ann (L _ bindings) body -> insertBindingIntoLet position ann bindings body binding
    _ -> Left NotABindingContainer

-- | Replace a binding with a parsed binding node in a set or let container.
replaceBinding :: Int -> LBinding -> Expr -> EditResult Expr
replaceBinding idx replacement expr =
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
insertListElement :: ListInsertPosition -> LExpr -> Expr -> EditResult Expr
insertListElement position element expr =
  case expr of
    NixList ann xs -> do
      idx <- normalizeListInsertIndex position (length xs)
      rebuildListLayout ann (insertAt idx element xs)
    _ -> Left NotAList

-- | Replace a list element with a parsed expression node.
replaceListElement :: Int -> LExpr -> Expr -> EditResult Expr
replaceListElement idx replacement expr =
  case expr of
    NixList ann xs -> do
      target <- elementAt idx xs
      let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
      rebuildListLayout ann (replaceAt idx replacement' xs)
    _ -> Left NotAList

insertSetBindingText :: BindingInsertPosition -> Text -> Expr -> EditResult Expr
insertSetBindingText position bindingText expr = do
  newBinding <- parseBinding bindingText
  insertSetBinding position newBinding expr

-- | Insert a binding parsed from source text into a set or @let@ container.
insertBindingAt :: BindingInsertPosition -> Text -> Expr -> EditResult Expr
insertBindingAt position bindingText expr = do
  binding <- parseBinding bindingText
  insertBinding position binding expr

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
  replacement <- parseBinding bindingText
  replaceSetBinding idx replacement expr

-- | Replace the RHS of a normal binding using source text for the new value.
replaceBindingValue :: Int -> Text -> Expr -> EditResult Expr
replaceBindingValue idx valueText expr = do
  replacementExpr <- parseExpr valueText
  replaceBindingValueNodeAt idx replacementExpr expr

-- | Insert a list element parsed from source text.
insertListElementAt :: ListInsertPosition -> Text -> Expr -> EditResult Expr
insertListElementAt position elemText expr = do
  element <- parseExpr elemText
  insertListElement position element expr

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
  replacementKey <- parseAttrKey keyText
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
