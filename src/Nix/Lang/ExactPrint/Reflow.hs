module Nix.Lang.ExactPrint.Reflow where

import Data.Text (Text)
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Span
import Nix.Lang.Utils

data Flow
  = FlowInline
  | FlowMultiline

reflow :: Flow -> (a -> Text) -> (RenderCursor -> Located a -> Located a) -> RenderCursor -> [Located a] -> [Located a]
reflow flow renderItem moveItem startCursor = snd . foldl step (startCursor, [])
  where
    step (cursor, acc) item =
      let moved = moveItem cursor item
          next = advanceItem flow renderItem cursor moved
       in (next, acc <> [moved])

advanceItem :: Flow -> (a -> Text) -> RenderCursor -> Located a -> RenderCursor
advanceItem flow renderItem startCursor item =
  case flow of
    FlowInline -> advanceCursor endCursor " "
    FlowMultiline -> RenderCursor (rcLine endCursor + 1) (rcColumn startCursor)
  where
    endCursor = advanceCursor startCursor (renderItem (unLoc item))

closeAfter :: Flow -> (a -> Text) -> SrcSpan -> [Located a] -> RenderCursor
closeAfter flow renderItem oldClose items =
  case reverse items of
    [] -> cursorAtSpanStart oldClose
    lastItem : _ ->
      case flow of
        FlowInline -> advanceCursor (cursorAtSpanStart (getLoc lastItem)) (renderItem (unLoc lastItem) <> " ")
        FlowMultiline ->
          let endCursor = advanceCursor (cursorAtSpanStart (getLoc lastItem)) (renderItem (unLoc lastItem))
           in RenderCursor (rcLine endCursor + 1) (srcSpanStartColumn oldClose)
