-- | Result and error types used while preparing edited trees for exact printing.
module Nix.Lang.ExactPrint.Prepare.Types
  ( EPResult,
    EPError (..),
    BindingInsertPosition (..),
    ListInsertPosition (..),
  )
where

import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Result type used by pure exact-print preparation operations.
--
-- The exact-print preparation pipeline is intentionally modeled as a pure @Either@ pipeline:
-- operations either produce a repaired tree or an 'EPError'.
type EPResult = Either EPError

-- | Errors raised by internal exact-print preparation operations.
data EPError
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

-- | Where to insert a binding in a set or @let@ binding list.
data BindingInsertPosition
  = InsertBindingAt Int
  | AppendBinding
  deriving (Show, Eq)

-- | Where to insert an element in a list expression.
data ListInsertPosition
  = InsertListElementAt Int
  | AppendListElement
  deriving (Show, Eq)

--------------------------------------------------------------------------------
