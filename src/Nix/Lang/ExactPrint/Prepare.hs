-- | Centralized exact-print preparation.
--
-- This module owns the transition from a structurally valid AST to an
-- exact-print-ready AST. Callers may preserve or provide layout metadata,
-- but they are not required to maintain a globally coherent exact-print invariant themselves.
module Nix.Lang.ExactPrint.Prepare
  ( prepareExpr,
    prepareBinding,
    prepareAttrPath,
    prepareFuncPat,
  )
where

import Nix.Lang.ExactPrint.Internal.Types (EPResult)
import Nix.Lang.ExactPrint.Internal.Repair
import Nix.Lang.Types.Parsed

-- | Prepare an expression into an exact-print-ready form.
prepareExpr :: Expr -> EPResult Expr
prepareExpr = repairExprLayout

-- | Prepare a binding into an exact-print-ready form.
prepareBinding :: Binding -> EPResult Binding
prepareBinding = repairBindingLayout

-- | Prepare an attribute path into an exact-print-ready form.
prepareAttrPath :: AttrPath -> EPResult AttrPath
prepareAttrPath = repairAttrPathLayout

-- | Prepare a function pattern into an exact-print-ready form.
prepareFuncPat :: FuncPat -> EPResult FuncPat
prepareFuncPat = repairFuncPatLayout
