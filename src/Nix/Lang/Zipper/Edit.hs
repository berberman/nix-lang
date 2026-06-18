module Nix.Lang.Zipper.Edit
  ( replaceExpr,
    replaceIntLiteral,
  )
where

import Data.Bifunctor (first)
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Nix.Lang.Zipper
import Nix.Lang.Zipper.ExactPrint

replaceExpr :: LExpr -> Focus Expr Expr -> CloseResult LExpr
replaceExpr replacement = closeExpr . replaceFocus replacement

replaceIntLiteral :: Integer -> Focus Expr Lit -> CloseResult LExpr
replaceIntLiteral value focused = do
  exprFocus <- first CloseStructuralError $ rebuildLitParent (modifyFocus rewriteIntegerLiteral focused)
  closeExpr exprFocus
  where
    rewriteIntegerLiteral (L l _) = L l (NixInteger NoExtF value)

rebuildLitParent :: Focus Expr Lit -> ZipperResult (Focus Expr Expr)
rebuildLitParent (Focus lit (Step parentPath parent route)) =
  case route of
    Required ExprLit -> replaceChildAt (Required ExprLit) lit (Focus parent parentPath)
    _ ->
      Left
        RouteMismatch
          { mismatchRoute = "literal expression payload",
            actualConstructor = routeLabel route,
            parentSpan = getLoc parent
          }
