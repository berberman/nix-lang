{-# LANGUAGE RankNTypes #-}

-- | Shared span, cursor, and translation helpers for exact-print preparation.
module Nix.Lang.ExactPrint.Prepare.Utils
  ( bindingSpan,
    bindingRenderSpan,
    bindingComments,
    staticAttrKeyText,
    staticAttrPathText,
    renderDoubleQuotedSourceText,
    renderIndentedStringSourceText,
    shiftSpanRight,
    shiftComments,
    applyDeltaToAnchor,
    tokenSpanAt,
    setAnnSpan,
    preserveGapTarget,
    preserveGapTargetForSetPat,
    preserveGapSpan,
    spanStartCursor,
    spanEndCursor,
    cursorSpan,
    textSpanAt,
    cursorFallbackFile,
    cursorFile,
    lastMay,
    listSpan,
    listSpanOr,
    translateFromTo,
  )
where

import Data.Data (Data)
import Data.Generics (everywhere, mkT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Types.Ps
import Nix.Lang.Utils

--------------------------------------------------------------------------------

-- | Recover the full render span of a binding.
--
-- This prefers token-backed spans when they are available so rebuilt bindings
-- can preserve delimiter ownership precisely.
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

-- | Extend 'bindingSpan' to include comments owned before the binding.
bindingRenderSpan :: Binding -> SrcSpan
bindingRenderSpan binding =
  foldr combineSrcSpans (bindingSpan binding) (getLoc <$> priorComments (bindingComments binding))

-- | Get the comments owned by a binding node.
bindingComments :: Binding -> NodeComments
bindingComments = \case
  NixNormalBinding ann _ _ -> annComments ann
  NixInheritBinding ann _ _ -> annComments ann

-- | Get the concrete text of a static attribute key.
staticAttrKeyText :: AttrKey -> Maybe Text
staticAttrKeyText = \case
  NixStaticAttrKey _ (L _ key) -> Just key
  _ -> Nothing

-- | Get the concrete text segments of a fully static attribute path.
staticAttrPathText :: AttrPath -> Maybe [Text]
staticAttrPathText (NixAttrPath _ keys) = traverse (staticAttrKeyText . unLoc) keys

--------------------------------------------------------------------------------

-- | Move a list of comments from one span anchor to another.
--
-- Comment spans are translated structurally, preserving their relative offsets
-- from the owning token or delimiter.
shiftComments :: SrcSpan -> SrcSpan -> [Located Comment] -> [Located Comment]
shiftComments oldSpan newSpan = fmap (translateFromTo oldSpan newSpan)

-- | Shift a span horizontally on the same line by a fixed number of columns.
shiftSpanRight :: Int -> SrcSpan -> SrcSpan
shiftSpanRight delta span' =
  mkSrcSpan
    (srcSpanFilename span')
    (srcSpanStartLine span', srcSpanStartColumn span' + delta)
    (srcSpanEndLine span', srcSpanEndColumn span' + delta)

-- | Render the exact parsed source of a double-quoted string.
renderDoubleQuotedSourceText :: SourceText -> Text
renderDoubleQuotedSourceText (SourceText src) = "\"" <> src <> "\""

-- | Render the exact parsed source of an indented string.
renderIndentedStringSourceText :: SourceText -> Text
renderIndentedStringSourceText (SourceText src) = "''" <> src <> "''"

--------------------------------------------------------------------------------

-- | Apply a relative delta to the end of an anchor span.
--
-- This is the inverse of storing token positions as deltas: once a new anchor is
-- known, the target span can be reconstructed deterministically from the delta.
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

--------------------------------------------------------------------------------

-- | Place a token at a concrete render cursor.
--
-- The token width comes from the concrete token text so synthesized spans match
-- the way exact printing will emit the token.
tokenSpanAt :: RenderCursor -> AnnToken -> SrcSpan
tokenSpanAt cursor tok =
  mkSrcSpan fileName (rcLine cursor, rcColumn cursor) (rcLine cursor, rcColumn cursor + tokenWidth tok)
  where
    fileName = maybe "<edited>" srcSpanFilename (annTokenSrcSpan tok)

--------------------------------------------------------------------------------

-- | Preserve the relative gap from an old anchor to an old target at a new anchor.
--
-- This is the core cursor-preservation primitive used throughout repair: it says
-- "where should the new target start if it should keep the same relative gap it
-- had from the old anchor?".
preserveGapTarget :: SrcSpan -> SrcSpan -> SrcSpan -> RenderCursor
preserveGapTarget oldAnchor oldTarget newAnchor = spanStartCursor (applyDeltaToAnchor newAnchor (deltaFromAnchor oldAnchor oldTarget))

-- | Preserve the default-expression gap for a set-pattern binding.
preserveGapTargetForSetPat :: SetPatBinding -> LBinderName -> LExpr -> RenderCursor
preserveGapTargetForSetPat binding var' oldDef =
  case aspbQuestion (nspbAnn binding) of
    Just qTok -> preserveGapTarget (expectTokenSpan "set pattern question" qTok) (getLoc oldDef) (preserveGapSpan (getLoc (nspbVar binding)) (expectTokenSpan "set pattern question" qTok) var')
    Nothing -> spanStartCursor (getLoc oldDef)

-- | Preserve the relative placement of a target span at a new anchor node.
--
-- Unlike 'preserveGapTarget', this returns a span rather than a cursor and is
-- typically used for tokens whose concrete width is already known.
preserveGapSpan :: SrcSpan -> SrcSpan -> Located a -> SrcSpan
preserveGapSpan oldAnchor oldTarget newAnchor = applyDeltaToAnchor (getLoc newAnchor) (deltaFromAnchor oldAnchor oldTarget)

--------------------------------------------------------------------------------

-- | Convert a span start into a render cursor.
spanStartCursor :: SrcSpan -> RenderCursor
spanStartCursor = cursorAtSpanStart

-- | Convert a span end into a render cursor.
spanEndCursor :: SrcSpan -> RenderCursor
spanEndCursor span' = RenderCursor (srcSpanEndLine span') (srcSpanEndColumn span')

-- | Create a one-column span at a render cursor.
cursorSpan :: RenderCursor -> String -> SrcSpan
cursorSpan cursor file = mkSrcSpan file (rcLine cursor, rcColumn cursor) (rcLine cursor, rcColumn cursor + 1)

-- | Compute the concrete span occupied by some rendered text at a cursor.
textSpanAt :: String -> RenderCursor -> Text -> SrcSpan
textSpanAt file cursor txt =
  let end = advanceCursor cursor txt
   in mkSrcSpan file (rcLine cursor, rcColumn cursor) (rcLine end, rcColumn end)

-- | Fallback pseudo-file name used for synthesized leaf spans.
cursorFallbackFile :: String
cursorFallbackFile = "<edited>"

-- | Get the file associated with a located value.
cursorFile :: Located a -> String
cursorFile locd = srcSpanFilename (getLoc locd)

--------------------------------------------------------------------------------

-- | Safe @last@ as a 'Maybe'.
lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)

-- | Span covering an entire non-empty located list.
listSpan :: [Located a] -> SrcSpan
listSpan [] = mkSrcSpan "<edited>" (1, 1) (1, 1)
listSpan xs = foldr1 combineSrcSpans (getLoc <$> xs)

-- | 'listSpan' with a fallback for the empty case.
listSpanOr :: SrcSpan -> [Located a] -> SrcSpan
listSpanOr fallback [] = fallback
listSpanOr _ xs = listSpan xs

--------------------------------------------------------------------------------

-- | Translate every span in a tree from one anchor span to another.
--
-- This is the core structural move operation used by both tree-moving callers and
-- layout repair. The algorithm keeps every point's line/column offset relative
-- to the original anchor, then replays that offset from the target anchor.
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

--------------------------------------------------------------------------------

tokenWidth :: AnnToken -> Int
tokenWidth tok = max 1 . T.length $ fromMaybe " " (showToken (annToken tok))
