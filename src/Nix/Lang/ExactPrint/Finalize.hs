-- | Centralized exact-print finalization.
--
-- This module owns the transition from a structurally valid AST to an
-- exact-print-ready AST. Callers such as zippers, builders, or quasiquoters may
-- preserve or provide layout metadata, but they are not required to maintain a
-- globally coherent exact-print invariant themselves.
--
-- Finalization has two goals:
--
-- * preserve existing layout when parser-originated metadata is still coherent,
-- * and make layout hints optional by synthesizing any missing exact-print
--   details inside the exact-print subsystem.
module Nix.Lang.ExactPrint.Finalize
  ( LayoutMode (..),
    LayoutHints (..),
    noLayoutHints,
    finalizeExpr,
    finalizeBinding,
    finalizeAttrPath,
    finalizeFuncPat,
  )
where

import Nix.Lang.ExactPrint.Edit.Rebuild
import Nix.Lang.ExactPrint.Edit.Types (EditResult)
import Nix.Lang.ExactPrint.Repair
import Nix.Lang.Span
import Nix.Lang.Types

-- | High-level layout policy used when exact-print metadata must be finalized.
data LayoutMode
  = -- | Preserve existing spans/deltas whenever the current repair/rebuild logic
    -- can do so coherently.
    PreserveLayoutHints
  | -- | Prefer canonical rebuilt anchors for sequence containers instead of
    -- preserving their previous first-child slot.
    CanonicalLayout
  deriving (Show, Eq)

-- | Optional caller-provided layout preferences.
--
-- These are intentionally narrow. Downstream users may express desired layout,
-- but are not required to provide fully formed annotation geometry.
newtype LayoutHints = LayoutHints
  { layoutMode :: LayoutMode
  }
  deriving (Show, Eq)

-- | Default finalization behavior: preserve existing layout metadata when
-- possible.
noLayoutHints :: LayoutHints
noLayoutHints = LayoutHints {layoutMode = PreserveLayoutHints}

-- | Finalize an expression into an exact-print-ready form.
--
-- This is the intended ownership boundary for layout correctness. Structural
-- editing APIs may return expressions with stale or approximate metadata; this
-- pass centralizes the repair/rebuild work required before exact printing.
finalizeExpr :: LayoutHints -> Expr -> EditResult Expr
finalizeExpr hints expr =
  case layoutMode hints of
    PreserveLayoutHints -> repairExprLayout expr
    CanonicalLayout ->
      case expr of
        NixSet ann kind (L _ bindings) ->
          rebuildSetLayoutWithAnchor AnchorStartAtFirstSlot ann kind bindings
        NixLet ann (L _ bindings) body ->
          rebuildLetLayoutWithAnchor AnchorStartAtFirstSlot ann bindings body
        NixList ann elems ->
          rebuildListLayoutWithAnchor AnchorStartAtFirstSlot ann elems
        _ -> repairExprLayout expr

-- | Finalize a binding into an exact-print-ready form.
finalizeBinding :: LayoutHints -> Binding -> EditResult Binding
finalizeBinding _ = repairBindingLayout

-- | Finalize an attribute path into an exact-print-ready form.
finalizeAttrPath :: LayoutHints -> AttrPath -> EditResult AttrPath
finalizeAttrPath _ = repairAttrPathLayout

-- | Finalize a function pattern into an exact-print-ready form.
finalizeFuncPat :: LayoutHints -> FuncPat -> EditResult FuncPat
finalizeFuncPat _ = repairFuncPatLayout
