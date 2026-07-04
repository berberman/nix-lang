{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Source span and located-value types.
--
-- These are the location wrappers used throughout the parsed and exact-printing
-- parts of the library.
module Nix.Lang.Span where

import Data.Data (Data)

data SrcSpan = SrcSpan
  { srcSpanFilename :: String,
    srcSpanStartLine :: Int,
    srcSpanStartColumn :: Int,
    srcSpanEndLine :: Int,
    srcSpanEndColumn :: Int
  }
  deriving (Eq, Data)

instance Show SrcSpan where
  show SrcSpan {..} =
    srcSpanFilename
      <> ":"
      <> if srcSpanStartLine == srcSpanEndLine
        then
          show srcSpanEndLine
            <> ":"
            <> if srcSpanStartColumn == srcSpanEndColumn
              then show srcSpanEndColumn
              else show srcSpanStartColumn <> "-" <> show srcSpanEndColumn
        else show (srcSpanStartLine, srcSpanStartColumn) <> "-" <> show (srcSpanEndLine, srcSpanEndColumn)

instance Ord SrcSpan where
  a `compare` b = case (srcSpanStartLine a, srcSpanStartColumn a)
    `compare` (srcSpanStartLine b, srcSpanStartColumn b) of
    EQ -> (srcSpanEndLine a, srcSpanEndColumn a) `compare` (srcSpanEndLine b, srcSpanEndColumn b)
    x -> x

data Located a = L SrcSpan a
  deriving (Eq, Show, Ord, Data, Functor, Foldable, Traversable)
