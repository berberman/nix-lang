-- | Shared types for exact-print-aware edit operations.
module Nix.Lang.ExactPrint.Edit.Types
  ( EditResult,
    EditError (..),
    BindingInsertPosition (..),
    ListInsertPosition (..),
  )
where

import Data.Text (Text)

-- | Result type used by pure edit and repair operations.
--
-- The edit subsystem is intentionally modeled as a pure @Either@ pipeline:
-- operations either produce a repaired tree or an 'EditError'.
type EditResult = Either EditError

-- | Errors raised by exact-print edit and repair operations.
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
