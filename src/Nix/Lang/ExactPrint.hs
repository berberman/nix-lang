{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Nix.Lang.ExactPrint
  ( ExactPrint (..),
    ExactPrintError (..),
    exactPrint,
    renderExactPrint,
    renderExactPrintM,
    renderExactDoc,
    renderDoc,
    module Nix.Lang.ExactPrint.Edit,
    module Nix.Lang.ExactPrint.Operations,
  )
where

import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint.Edit
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------

data ExactPrintError
  = MissingTokenSpan Text AnnToken
  | EmptyAttrPath
  | MismatchedAttrPathDots Int Int
  | MismatchedSetPatCommas Int Int
  | MissingSetPatQuestion SrcSpan
  deriving (Show, Eq)

-- | The printer accumulates exact output as concrete text chunks and tracks the
-- current logical cursor used to interpret relative gaps and token deltas.
data ExactPrinterState = ExactPrinterState
  { epsChunks :: [Text],
    epsCursor :: Maybe RenderCursor
  }

newtype ExactM a = ExactM
  { runExactM :: StateT ExactPrinterState (Except ExactPrintError) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError ExactPrintError, MonadState ExactPrinterState)

--------------------------------------------------------------------------------

class ExactPrint a where
  exactPrintM :: a -> ExactM ()

--------------------------------------------------------------------------------

exactPrint :: (ExactPrint a) => a -> Doc ann
exactPrint = either (error . show) pretty . renderExactPrintM

renderExactPrint :: (ExactPrint a) => a -> Text
renderExactPrint = either (error . show) id . renderExactPrintM

renderExactPrintM :: (ExactPrint a) => a -> Either ExactPrintError Text
renderExactPrintM x = finishPrinterState . snd <$> runExactPrinter (exactPrintM x)

renderExactDoc :: (ExactPrint a) => a -> Either ExactPrintError (Doc ann)
renderExactDoc = fmap pretty . renderExactPrintM

renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

emptyPrinterState :: ExactPrinterState
emptyPrinterState = ExactPrinterState [] Nothing

runExactPrinter :: ExactM a -> Either ExactPrintError (a, ExactPrinterState)
runExactPrinter action = runExcept (runStateT (runExactM action) emptyPrinterState)

finishPrinterState :: ExactPrinterState -> Text
finishPrinterState = T.concat . reverse . epsChunks

exactPrintLocatedM :: (ExactPrint a) => Located a -> ExactM ()
exactPrintLocatedM = exactPrintM . unLoc

--------------------------------------------------------------------------------

-- | Emit a raw text fragment into the printer state.
emitText :: Text -> ExactM ()
emitText txt =
  modify' $ \st@ExactPrinterState {epsChunks, epsCursor} ->
    st
      { epsChunks = txt : epsChunks,
        epsCursor = fmap (`advanceCursor` txt) epsCursor
      }

setCursor :: RenderCursor -> ExactM ()
setCursor cursor = modify' $ \st -> st {epsCursor = Just cursor}

-- | Ensure the printer has a current cursor, initializing it when rendering is
-- starting from an otherwise anchor-free context.
ensureCursor :: RenderCursor -> ExactM RenderCursor
ensureCursor fallback = do
  mCursor <- gets epsCursor
  case mCursor of
    Just cursor -> pure cursor
    Nothing -> setCursor fallback >> pure fallback

-- | Move the output cursor to the start of a target span by emitting the exact
-- gap implied by the current cursor and that span.
emitGapToSpan :: SrcSpan -> ExactM ()
emitGapToSpan span' = do
  mCursor <- gets epsCursor
  case mCursor of
    Nothing -> setCursor (cursorAtSpanStart span')
    Just cursor -> emitText (renderGapFromCursorToSpanText cursor span')

-- | Emit text whose first character is anchored at a particular span start.
emitAtSpan :: SrcSpan -> Text -> ExactM ()
emitAtSpan span' txt = emitGapToSpan span' >> emitText txt

emitDelta :: DeltaPos -> ExactM ()
emitDelta delta = do
  _ <- ensureCursor (RenderCursor 1 1)
  emitText (renderGapFromDeltaText delta)

-- | Recover the emitted surface text for a token from its annotation identity.
tokenTextM :: Text -> AnnToken -> ExactM Text
tokenTextM label tok =
  case showToken (annToken tok) of
    Just txt -> pure txt
    Nothing -> throwError $ MissingTokenSpan (label <> ": token text unavailable") tok

-- | Emit a token using the canonical text associated with its annotation.
emitToken :: Text -> AnnToken -> ExactM ()
emitToken label tok = do
  txt <- tokenTextM label tok
  emitTokenText label tok txt

-- | Emit a token using explicit text supplied by the caller.
emitTokenText :: Text -> AnnToken -> Text -> ExactM ()
emitTokenText _ tok txt = do
  case annTokenPos tok of
    AnnSpan src -> emitGapToSpan src
    AnnDelta delta -> emitDelta delta
  emitText txt

--------------------------------------------------------------------------------

emitCommentAt :: Located Comment -> ExactM ()
emitCommentAt (L span' comment) = emitAtSpan span' (renderCommentText comment)

emitComments :: [Located Comment] -> ExactM ()
emitComments = mapM_ emitCommentAt

emitPriorCommentsTo :: SrcSpan -> [Located Comment] -> ExactM ()
emitPriorCommentsTo target comments = do
  emitComments comments
  unless (null comments) (emitGapToSpan target)

emitFollowingComments :: [Located Comment] -> ExactM ()
emitFollowingComments = emitComments

-- | Emit a node together with the comments it owns.
emitWrappedNode :: (HasAnnCommon a) => SrcSpan -> a -> ExactM () -> ExactM ()
emitWrappedNode target ann body = do
  let comments = annComments ann
  emitPriorCommentsTo target (priorComments comments)
  body
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

instance ExactPrint Expr where
  exactPrintM = \case
    NixVar ann ident -> emitWrappedNode (getLoc ident) ann $ emitAtSpan (getLoc ident) (unLoc ident)
    NixLit ann lit -> emitWrappedNode (getLoc lit) ann $ emitGapToSpan (getLoc lit) >> exactPrintM (unLoc lit)
    NixPar ann (L _ x) -> renderParM ann x
    NixString ann str -> emitWrappedNode (getLoc str) ann $ emitAtSpan (getLoc str) (renderStringText (unLoc str))
    NixPath ann path -> emitWrappedNode (getLoc path) ann $ emitAtSpan (getLoc path) (renderPathText (unLoc path))
    NixEnvPath ann path -> renderEnvPathM ann path
    NixLam ann (L _ pat) (L _ x) -> renderLamM ann pat x
    NixApp ann (L _ f) (L _ x) -> emitWrappedNode (exprSpan f) ann $ exactPrintM f >> exactPrintM x
    NixBinApp ann op (L _ x) (L _ y) -> renderBinAppM ann op x y
    NixNotApp ann (L _ x) -> renderPrefixAppM ann "!" x
    NixNegApp ann (L _ x) -> renderPrefixAppM ann "-" x
    NixList ann xs -> renderListM ann xs
    NixSet ann NixSetRecursive (L _ bindings) -> renderSetM ann True bindings
    NixSet ann NixSetNonRecursive (L _ bindings) -> renderSetM ann False bindings
    NixLet ann (L _ bindings) (L _ x) -> renderLetM ann bindings x
    NixHasAttr ann (L _ x) (L _ p) -> renderHasAttrM ann x p
    NixSelect ann (L _ x) (L _ p) mx -> renderSelectM ann x p mx
    NixIf ann (L _ cond) (L _ t) (L _ f) -> renderIfM ann cond t f
    NixWith ann (L _ scope) (L _ x) -> renderWithM ann scope x
    NixAssert ann (L _ assertion) (L _ x) -> renderAssertM ann assertion x

instance ExactPrint Lit where
  exactPrintM = emitText . renderLitText

instance ExactPrint AttrPath where
  exactPrintM path@(NixAttrPath ann keys) =
    emitWrappedNode (attrPathSpan path) ann (renderAttrPathM ann keys)

instance ExactPrint AttrKey where
  exactPrintM = emitText . renderAttrKeyText

instance ExactPrint Binding where
  exactPrintM = \case
    NixNormalBinding ann (L _ path) (L _ x) -> renderNormalBindingM ann path x
    NixInheritBinding ann mScope names -> renderInheritBindingM ann mScope names

instance ExactPrint SetPatAs where
  exactPrintM = renderSetPatAsM

instance ExactPrint FuncPat where
  exactPrintM = \case
    NixVarPat ann ident -> emitWrappedNode (getLoc ident) ann $ emitAtSpan (getLoc ident) (unLoc ident)
    pat@(NixSetPat ann ellipses mAs params) ->
      emitWrappedNode (funcPatBodySpan pat) ann $ renderSetPatM ann ellipses mAs params

--------------------------------------------------------------------------------

renderLitText :: Lit -> Text
renderLitText = \case
  NixUri _ uri -> uri
  NixInteger _ int -> T.pack (show int)
  NixFloat _ float -> T.pack (show float)
  NixBoolean _ True -> "true"
  NixBoolean _ False -> "false"
  NixNull _ -> "null"

renderStringText :: NString -> Text
renderStringText = \case
  NixDoubleQuotesString src _ -> renderDoubleQuotedSourceText src
  NixDoubleSingleQuotesString src _ -> renderIndentedStringSourceText src

renderPathText :: Path -> Text
renderPathText = \case
  NixLiteralPath _ path -> path
  NixInterpolPath src _ -> renderPathSourceText src

renderAttrKeyText :: AttrKey -> Text
renderAttrKeyText = \case
  NixStaticAttrKey _ (L _ x) -> x
  NixDynamicStringAttrKey src _ -> renderDoubleQuotedSourceText src
  NixDynamicInterpolAttrKey src _ -> renderDynamicInterpolSourceText src

renderSetPatAsM :: SetPatAs -> ExactM ()
renderSetPatAsM NixSetPatAs {..} =
  emitWrappedNode (setPatAsRenderSpan nspaAnn nspaVar nspaLocation) nspaAnn $ case nspaLocation of
    NixSetPatAsLeading -> do
      emitAtSpan (getLoc nspaVar) (unLoc nspaVar)
      emitToken "set pattern at" (aspaAt nspaAnn)
    NixSetPatAsTrailing -> do
      emitToken "set pattern at" (aspaAt nspaAnn)
      emitAtSpan (getLoc nspaVar) (unLoc nspaVar)

instance ExactPrint SetPatBinding where
  exactPrintM NixSetPatBinding {..} = do
    emitAtSpan (getLoc nspbVar) (unLoc nspbVar)
    case nspbDefault of
      Nothing -> pure ()
      Just defExpr -> case aspbQuestion nspbAnn of
        Just qTok -> emitToken "set pattern question" qTok >> exactPrintLocatedM defExpr
        Nothing -> throwError $ MissingSetPatQuestion (getLoc nspbVar)

--------------------------------------------------------------------------------

renderListM :: AnnListNode -> [LExpr] -> ExactM ()
renderListM ann xs = do
  let comments = annComments ann
  emitPriorCommentsTo (expectTokenSpan "list open bracket" (alnOpenS ann)) (priorComments comments)
  emitToken "list open bracket" (alnOpenS ann)
  mapM_ exactPrintLocatedM xs
  emitFollowingComments (followingComments comments)
  emitToken "list close bracket" (alnCloseS ann)

--------------------------------------------------------------------------------

renderSetM :: AnnSet -> Bool -> [LBinding] -> ExactM ()
renderSetM ann _ bindings = do
  let comments = annComments ann
      openAnchor = maybe (expectTokenSpan "set open brace" (asOpenC ann)) (expectTokenSpan "rec keyword") (asRec ann)
  emitPriorCommentsTo openAnchor (priorComments comments)
  case asRec ann of
    Just recTok -> emitToken "rec keyword" recTok
    Nothing -> pure ()
  emitToken "set open brace" (asOpenC ann)
  mapM_ exactPrintLocatedM bindings
  emitFollowingComments (followingComments comments)
  emitToken "set close brace" (asCloseC ann)

--------------------------------------------------------------------------------

renderEnvPathM :: AnnEnvPathNode -> Located Text -> ExactM ()
renderEnvPathM ann path = do
  let comments = annComments ann
  emitPriorCommentsTo (expectTokenSpan "env path open" (aenvOpen ann)) (priorComments comments)
  emitToken "env path open" (aenvOpen ann)
  emitAtSpan (getLoc path) (unLoc path)
  emitFollowingComments (followingComments comments)
  emitToken "env path close" (aenvClose ann)

--------------------------------------------------------------------------------

renderLamM :: AnnLamNode -> FuncPat -> Expr -> ExactM ()
renderLamM ann pat body = do
  let comments = annComments ann
  emitPriorCommentsTo (funcPatRenderSpan pat) (priorComments comments)
  exactPrintM pat
  emitToken "lambda colon" (alamColon ann)
  exactPrintM body
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderBinAppM :: AnnBinAppNode -> BinaryOp -> Expr -> Expr -> ExactM ()
renderBinAppM ann op lhs rhs = do
  let comments = annComments ann
  emitPriorCommentsTo (exprSpan lhs) (priorComments comments)
  exactPrintM lhs
  emitTokenText "binary operator" (abinOperator ann) (showBinOP op)
  exactPrintM rhs
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderPrefixAppM :: AnnPrefixNode -> Text -> Expr -> ExactM ()
renderPrefixAppM ann tok expr = do
  let comments = annComments ann
  emitPriorCommentsTo (expectTokenSpan "prefix operator" (apfxToken ann)) (priorComments comments)
  emitTokenText "prefix operator" (apfxToken ann) tok
  exactPrintM expr
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderAttrPathM :: AnnAttrPath -> [LAttrKey] -> ExactM ()
renderAttrPathM ann keys =
  case keys of
    [] -> throwError EmptyAttrPath
    first : rest ->
      case aapDots ann of
        dotTok : moreDots | length (aapDots ann) == length keys -> do
          emitToken "attr path dot" dotTok
          emitGapToSpan (getLoc first)
          exactPrintM (unLoc first)
          emitAttrPathTail moreDots rest
        dots -> do
          emitGapToSpan (getLoc first)
          exactPrintM (unLoc first)
          emitAttrPathTail dots rest
  where
    emitAttrPathTail [] [] = pure ()
    emitAttrPathTail (dotTok : moreDots) (key : moreKeys) = do
      emitToken "attr path dot" dotTok
      emitGapToSpan (getLoc key)
      exactPrintM (unLoc key)
      emitAttrPathTail moreDots moreKeys
    emitAttrPathTail dots moreKeys = throwError $ MismatchedAttrPathDots (length dots) (length moreKeys)

--------------------------------------------------------------------------------

renderNormalBindingM :: AnnNormalBinding -> AttrPath -> Expr -> ExactM ()
renderNormalBindingM ann path expr = do
  let comments = annComments ann
  emitPriorCommentsTo (attrPathRenderSpan path) (priorComments comments)
  exactPrintM path
  emitToken "binding equals" (anbEqual ann)
  exactPrintM expr
  emitToken "binding semicolon" (anbSemicolon ann)
  emitFollowingComments (followingComments comments)

renderInheritBindingM :: AnnInheritBinding -> Maybe LExpr -> [LAttrKey] -> ExactM ()
renderInheritBindingM ann mScope names = do
  let comments = annComments ann
      inheritSpan = expectTokenSpan "inherit keyword" (aibInherit ann)
  emitPriorCommentsTo inheritSpan (priorComments comments)
  emitToken "inherit keyword" (aibInherit ann)
  maybe (pure ()) exactPrintLocatedM mScope
  mapM_ emitName names
  emitToken "inherit semicolon" (aibSemicolon ann)
  emitFollowingComments (followingComments comments)
  where
    emitName name = emitGapToSpan (getLoc name) >> exactPrintM (unLoc name)

--------------------------------------------------------------------------------

renderParM :: AnnParNode -> Expr -> ExactM ()
renderParM ann expr = do
  let comments = annComments ann
  emitPriorCommentsTo (expectTokenSpan "paren open" (apnOpenP ann)) (priorComments comments)
  emitToken "paren open" (apnOpenP ann)
  exactPrintM expr
  emitFollowingComments (followingComments comments)
  emitToken "paren close" (apnCloseP ann)

--------------------------------------------------------------------------------

renderLetM :: AnnLetNode -> [LBinding] -> Expr -> ExactM ()
renderLetM ann bindings expr = do
  let comments = annComments ann
      letSpan = expectTokenSpan "let keyword" (alLet ann)
  emitPriorCommentsTo letSpan (priorComments comments)
  emitToken "let keyword" (alLet ann)
  mapM_ exactPrintLocatedM bindings
  emitToken "in keyword" (alIn ann)
  exactPrintM expr
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderIfM :: AnnIfNode -> Expr -> Expr -> Expr -> ExactM ()
renderIfM ann cond thenExpr elseExpr = do
  let comments = annComments ann
      ifSpan = expectTokenSpan "if keyword" (aifIf ann)
  emitPriorCommentsTo ifSpan (priorComments comments)
  emitToken "if keyword" (aifIf ann)
  exactPrintM cond
  emitToken "then keyword" (aifThen ann)
  exactPrintM thenExpr
  emitToken "else keyword" (aifElse ann)
  exactPrintM elseExpr
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderWithM :: AnnWithNode -> Expr -> Expr -> ExactM ()
renderWithM ann scope expr = do
  let comments = annComments ann
      withSpan = expectTokenSpan "with keyword" (awWith ann)
  emitPriorCommentsTo withSpan (priorComments comments)
  emitToken "with keyword" (awWith ann)
  exactPrintM scope
  emitToken "with semicolon" (awSemicolon ann)
  exactPrintM expr
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderAssertM :: AnnAssertNode -> Expr -> Expr -> ExactM ()
renderAssertM ann assertion expr = do
  let comments = annComments ann
      assertSpan = expectTokenSpan "assert keyword" (aaAssert ann)
  emitPriorCommentsTo assertSpan (priorComments comments)
  emitToken "assert keyword" (aaAssert ann)
  exactPrintM assertion
  emitToken "assert semicolon" (aaSemicolon ann)
  exactPrintM expr
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderHasAttrM :: AnnHasAttr -> Expr -> AttrPath -> ExactM ()
renderHasAttrM ann expr path = do
  let comments = annComments ann
  emitPriorCommentsTo (exprSpan expr) (priorComments comments)
  exactPrintM expr
  emitToken "has-attr question" (ahaQuestion ann)
  exactPrintM path
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderSelectM :: AnnSelect -> Expr -> AttrPath -> Maybe LExpr -> ExactM ()
renderSelectM ann expr path def = do
  let comments = annComments ann
  emitPriorCommentsTo (exprSpan expr) (priorComments comments)
  exactPrintM expr
  exactPrintM path
  case def of
    Nothing -> pure ()
    Just defExpr -> case aslOr ann of
      Just orTok -> emitTokenText "select or" orTok "or" >> exactPrintLocatedM defExpr
      Nothing -> emitText " or " >> exactPrintLocatedM defExpr
  emitFollowingComments (followingComments comments)

--------------------------------------------------------------------------------

renderSetPatM :: AnnSetPatNode -> NixSetPatEllipses -> Maybe LSetPatAs -> [LSetPatBinding] -> ExactM ()
renderSetPatM ann ellipses mAs params = do
  case mAs of
    Just (L _ asPat@NixSetPatAs {nspaLocation = NixSetPatAsLeading}) -> exactPrintM asPat
    _ -> pure ()
  emitToken "set pattern open" (aspOpenC ann)
  renderSetPatEntriesM params (aspCommas ann) ellipses (aspEllipsis ann)
  emitToken "set pattern close" (aspCloseC ann)
  case mAs of
    Just (L _ asPat@NixSetPatAs {nspaLocation = NixSetPatAsTrailing}) -> exactPrintM asPat
    _ -> pure ()

renderSetPatEntriesM :: [LSetPatBinding] -> [AnnToken] -> NixSetPatEllipses -> Maybe AnnToken -> ExactM ()
renderSetPatEntriesM params commas ellipses ellipsisTok = do
  let (separatorCommas, trailingComma) = splitAt (max 0 (length params - 1)) commas
  renderBindings params separatorCommas
  case (ellipses, ellipsisTok, trailingComma) of
    (NixSetPatIsEllipses, Just tok, commaTok : _) -> emitToken "set pattern comma" commaTok >> emitToken "set pattern ellipsis" tok
    (NixSetPatIsEllipses, Just tok, []) -> emitToken "set pattern ellipsis" tok
    (NixSetPatIsEllipses, Nothing, _) -> pure ()
    (_, _, commaTok : _) -> emitToken "set pattern trailing comma" commaTok
    _ -> pure ()
  where
    renderBindings [] [] = pure ()
    renderBindings (param : rest) commaToks = do
      exactPrintM (unLoc param)
      case (rest, commaToks) of
        (next : more, commaTok : remaining) -> emitToken "set pattern comma" commaTok >> renderBindings (next : more) remaining
        ([], []) -> pure ()
        _ -> throwError $ MismatchedSetPatCommas (length commaToks) (length rest)
    renderBindings [] remaining = throwError $ MismatchedSetPatCommas (length remaining) 0

--------------------------------------------------------------------------------

renderCommentText :: Comment -> Text
renderCommentText = \case
  LineComment txt -> "#" <> txt
  BlockComment txt -> "/*" <> txt <> "*/"

renderDoubleQuotedSourceText :: SourceText -> Text
renderDoubleQuotedSourceText (SourceText src) = "\"" <> src <> "\""

renderIndentedStringSourceText :: SourceText -> Text
renderIndentedStringSourceText (SourceText src) = "''" <> src <> "''"

renderPathSourceText :: SourceText -> Text
renderPathSourceText (SourceText src) = src

renderDynamicInterpolSourceText :: SourceText -> Text
renderDynamicInterpolSourceText (SourceText src) = "${" <> src <> "}"
