module Nix.Lang.Zipper.ExactPrint
  ( CloseError (..),
    CloseResult,
    closeExpr,
    closeBinding,
    closeAttrPath,
    closeFuncPat,
  )
where

import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Generics (cast)
import Nix.Lang.ExactPrint.Edit
import Nix.Lang.ExactPrint.Edit.Geometry
import Nix.Lang.ExactPrint.Finalize
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Nix.Lang.Zipper

data CloseError
  = CloseStructuralError ZipperError
  | CloseEditError EditError
  deriving (Show, Eq)

type CloseResult = Either CloseError

data RepairPlan
  = PreserveNode
  | RepairExprPlan
  | RepairBindingPlan
  | RepairAttrPathPlan
  | RepairFuncPatPlan

liftStructural :: ZipperResult a -> CloseResult a
liftStructural = first CloseStructuralError

liftEditResult :: Either EditError a -> CloseResult a
liftEditResult = first CloseEditError

closeExpr :: Focus Expr focus -> CloseResult (Located Expr)
closeExpr focused =
  case path focused of
    Top -> do
      root <- liftStructural $ closeRaw focused
      repairExprNode root
    _ -> do
      root <- closeExprPath True (path focused) (focus focused)
      let expr = unLoc root
      pure $ L (exprSpan expr) expr

closeExprPath :: Bool -> ZPath Expr focus -> Located focus -> CloseResult (Located Expr)
closeExprPath _ Top root =
  case cast root of
    Just expr -> Right expr
    Nothing -> error "impossible: Expr root path closed with non-Expr focus"
closeExprPath isFirst (Step parentPath parent route) child = do
  parent' <- rebuildClosedExprAncestor isFirst route child parent
  closeExprPath False parentPath parent'

rebuildClosedExprAncestor :: (Data parent, Data child) => Bool -> ZRoute parent child -> Located child -> Located parent -> CloseResult (Located parent)
rebuildClosedExprAncestor isFirst route child parent = do
  child' <- reanchorClosedChild route child parent
  child'' <- if isFirst then finalizeClosedChild route child' else Right child'
  parent' <- liftStructural $ putRoute route child'' parent
  finalizeClosedParent route parent'

reanchorClosedChild :: (Data parent, Data child) => ZRoute parent child -> Located child -> Located parent -> CloseResult (Located child)
reanchorClosedChild route child parent = do
  oldChild <- liftStructural $ getRoute route parent
  pure (translateFromTo (getLoc child) (getLoc oldChild) child)

finalizeClosedChild :: (Data child) => ZRoute parent child -> Located child -> CloseResult (Located child)
finalizeClosedChild route = applyRepairPlan (childRepairPlan route)

finalizeClosedParent :: (Data parent) => ZRoute parent child -> Located parent -> CloseResult (Located parent)
finalizeClosedParent route = applyRepairPlan (parentRepairPlan route)

childRepairPlan :: ZRoute parent child -> RepairPlan
childRepairPlan = \case
  Required route -> case route of
    BindingP -> PreserveNode
    BindingV -> PreserveNode
    LamP -> RepairFuncPatPlan
    HasAttrP -> RepairAttrPathPlan
    SelectP -> RepairAttrPathPlan
    _ -> RepairExprPlan
  Optional route -> case route of
    SelectD -> RepairExprPlan
    InheritS -> PreserveNode
  Item route _ -> case route of
    SList -> RepairExprPlan
    _ -> PreserveNode

parentRepairPlan :: ZRoute parent child -> RepairPlan
parentRepairPlan = \case
  Required route -> case route of
    BindingP -> RepairBindingPlan
    BindingV -> RepairBindingPlan
    _ -> PreserveNode
  Optional route -> case route of
    SelectD -> PreserveNode
    InheritS -> RepairBindingPlan
  Item route _ -> case route of
    SSet -> RepairExprPlan
    SLet -> RepairExprPlan
    SList -> RepairExprPlan
    SAttrPath -> RepairAttrPathPlan
    SInherit -> RepairBindingPlan
    SSetPat -> RepairFuncPatPlan
    SString -> PreserveNode

applyRepairPlan :: (Data a) => RepairPlan -> Located a -> CloseResult (Located a)
applyRepairPlan plan =
  case plan of
    PreserveNode -> Right
    RepairExprPlan -> repairAs repairExprNode
    RepairBindingPlan -> repairAs repairBindingNode
    RepairAttrPathPlan -> repairAs repairAttrPathNode
    RepairFuncPatPlan -> repairAs repairFuncPatNode

repairAs :: (Data a, Data b) => (Located b -> CloseResult (Located b)) -> Located a -> CloseResult (Located a)
repairAs repair located =
  case cast located of
    Just typed -> do
      repaired <- repair typed
      case cast repaired of
        Just result -> Right result
        Nothing -> error "impossible: repaired node changed type"
    Nothing -> error "impossible: repair plan did not match node type"

repairLocatedNode :: (a -> EditResult a) -> (a -> SrcSpan) -> Located a -> CloseResult (Located a)
repairLocatedNode repair spanOf (L _ value) = do
  repaired <- liftEditResult $ repair value
  pure $ L (spanOf repaired) repaired

repairExprNode :: Located Expr -> CloseResult (Located Expr)
repairExprNode = repairLocatedNode (finalizeExpr noLayoutHints) exprSpan

repairBindingNode :: Located Binding -> CloseResult (Located Binding)
repairBindingNode = repairLocatedNode repairBindingLayout bindingSpan

repairAttrPathNode :: Located AttrPath -> CloseResult (Located AttrPath)
repairAttrPathNode = repairLocatedNode repairAttrPathLayout attrPathSpan

repairFuncPatNode :: Located FuncPat -> CloseResult (Located FuncPat)
repairFuncPatNode = repairLocatedNode repairFuncPatLayout funcPatBodySpan

closeWithRepair :: (a -> EditResult a) -> (a -> SrcSpan) -> Focus a focus -> CloseResult (Located a)
closeWithRepair repair spanOf focused = do
  root <- liftStructural $ closeRaw focused
  repairLocatedNode repair spanOf root

closeBinding :: Focus Binding focus -> CloseResult (Located Binding)
closeBinding = closeWithRepair repairBindingLayout bindingSpan

closeAttrPath :: Focus AttrPath focus -> CloseResult (Located AttrPath)
closeAttrPath = closeWithRepair repairAttrPathLayout attrPathSpan

closeFuncPat :: Focus FuncPat focus -> CloseResult (Located FuncPat)
closeFuncPat = closeWithRepair repairFuncPatLayout funcPatBodySpan
