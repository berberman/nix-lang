{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix.Lang.Edit
  ( Selector,
    Update,
    EPT.BindingInsertPosition (..),
    EditError (..),
    SelectError (..),
    root,
    (//),
    letBody,
    lambdaPattern,
    lambdaBody,
    appFunction,
    appArgument,
    ifCondition,
    ifThen,
    ifElse,
    withScope,
    withBody,
    assertCondition,
    assertBody,
    selectExpr,
    selectPath,
    selectDefault,
    hasAttrExpr,
    hasAttrPath,
    bindingAt,
    bindingByKey,
    bindingByPath,
    listElement,
    attrKeyAt,
    bindingPath,
    bindingValue,
    inheritScope,
    inheritKeyAt,
    inheritKey,
    literal,
    replace,
    modify,
    modifyLocated,
    replaceExprText,
    replaceBindingText,
    replaceAttrKeyText,
    setIntLiteral,
    renameAttrKey,
    insertBinding,
    insertBindingText,
    insertInheritKeyAt,
    insertInheritKeyTextAt,
    editExpr,
    editBinding,
    editAttrPath,
    editFuncPat,
  )
where

import Data.Data (Data, toConstr)
import Data.Generics (showConstr)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Annotation (AnnInheritBinding (..), AnnLetNode, alIn)
import Nix.Lang.Edit.Internal.TH
import qualified Nix.Lang.ExactPrint.Internal.Fragment as EPF
import Nix.Lang.ExactPrint.Internal.Geometry (bindingSpan, translateFromTo)
import Nix.Lang.ExactPrint.Internal.Rebuild (BindingSequenceAnchor (AnchorPreserveExisting, AnchorStartAtFirstSlot), rebuildLetLayout, rebuildLetLayoutWithAnchor, rebuildListLayoutWithAnchor, rebuildSetLayout, rebuildSetLayoutWithAnchor)
import qualified Nix.Lang.ExactPrint.Internal.Types as EPT
import Nix.Lang.ExactPrint.Operations (attrPathSpan, expectTokenSpan, exprSpan, funcPatBodySpan)
import Nix.Lang.ExactPrint.Prepare (prepareAttrPath, prepareBinding, prepareExpr, prepareFuncPat)
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Types.Parsed
import Nix.Lang.Utils

data SelectError
  = SelectorMismatch
      { selectorName :: Text,
        actualConstructor :: Text,
        selectionSpan :: SrcSpan
      }
  | OptionalAbsent
      { selectorName :: Text,
        selectionSpan :: SrcSpan
      }
  | IndexOutOfBounds
      { selectorName :: Text,
        index :: Int,
        itemCount :: Int,
        selectionSpan :: SrcSpan
      }
  | BindingNotFound
      { selectorName :: Text,
        bindingQuery :: [Text],
        selectionSpan :: SrcSpan
      }
  deriving (Show, Eq)

data EditError
  = SelectionError SelectError
  | UpdateError EPT.ExactPrintError
  | PrepareError EPT.ExactPrintError
  | ExpectedIntegerLiteral SrcSpan
  deriving (Show, Eq)

data Edited a
  = Raw (Located a)
  | Ready (Located a)

data Selected parent child = Selected
  { selected :: Located child,
    replaceSelected :: Edited child -> Either EditError (Edited parent)
  }

data Selector parent child = Selector
  { runSelector :: Located parent -> Either SelectError (Selected parent child)
  }

newtype Update focus = Update
  { runUpdate :: Located focus -> Either EditError (Edited focus)
  }

withSelectionError :: Either SelectError a -> Either EditError a
withSelectionError = mapLeft SelectionError

withUpdateError :: Either EPT.ExactPrintError a -> Either EditError a
withUpdateError = mapLeft UpdateError

withPrepareError :: Either EPT.ExactPrintError a -> Either EditError a
withPrepareError = mapLeft PrepareError

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f result =
  case result of
    Left err -> Left (f err)
    Right value -> Right value

root :: Selector a a
root = Selector {runSelector = \x -> Right Selected {selected = x, replaceSelected = Right}}

infixl 5 //

(//) :: Selector a b -> Selector b c -> Selector a c
Selector ab // Selector bc =
  Selector
    { runSelector = \a -> do
        Selected b putB <- ab a
        Selected c putC <- bc b
        pure
          Selected
            { selected = c,
              replaceSelected = \c' -> putC c' >>= putB
            }
    }

replace :: Located focus -> Update focus
replace replacement = Update (Right . const (Raw replacement))

modify :: (focus -> focus) -> Update focus
modify f = modifyLocated (fmap f)

modifyLocated :: (Located focus -> Located focus) -> Update focus
modifyLocated f = Update (Right . Raw . f)

replaceExprText :: Text -> Update Expr
replaceExprText source =
  Update $ \focused -> do
    replacement <- withUpdateError $ EPF.parseExpr source
    pure (Raw (translateFromTo (getLoc replacement) (getLoc focused) replacement))

replaceBindingText :: Text -> Update Binding
replaceBindingText source =
  Update $ \focused -> do
    replacement <- withUpdateError $ EPF.parseBinding source
    pure (Raw (translateFromTo (getLoc replacement) (getLoc focused) replacement))

replaceAttrKeyText :: Text -> Update AttrKey
replaceAttrKeyText source =
  Update $ \focused -> do
    replacement <- withUpdateError $ EPF.parseAttrKey source
    pure (Raw (translateFromTo (getLoc replacement) (getLoc focused) replacement))

setIntLiteral :: Integer -> Update Expr
setIntLiteral value = Update $ \focused ->
  case unLoc focused of
    NixLit ann (L litSpan (NixInteger _ _)) ->
      Right (Raw (L (getLoc focused) (NixLit ann (L litSpan (NixInteger NoExtF value)))))
    _ -> Left $ ExpectedIntegerLiteral (getLoc focused)

renameAttrKey :: Text -> Update AttrKey
renameAttrKey = replaceAttrKeyText

insertBinding :: EPT.BindingInsertPosition -> LBinding -> Update Expr
insertBinding position newBinding =
  Update $ \focused ->
    case focused of
      L _ (NixSet ann kind (L _ bindings)) -> do
        idx <- withUpdateError $ normalizeBindingInsertIndex position (length bindings)
        binding' <- withPrepareError $ prepareEditedRoot newBinding
        repaired <- withPrepareError $ rebuildSetLayoutWithAnchor (bindingInsertAnchor idx) ann kind (insertAt idx binding' bindings)
        pure (Ready (L (exprSpan repaired) repaired))
      L _ (NixLet ann (L _ bindings) body) -> do
        idx <- withUpdateError $ normalizeBindingInsertIndex position (length bindings)
        binding' <- withPrepareError $ prepareEditedRoot newBinding
        let insertedBindings = insertAt idx binding' bindings
            body' = prepareInsertedLetBody ann insertedBindings body
        repaired <- withPrepareError $ rebuildLetLayoutWithAnchor (bindingInsertAnchor idx) ann insertedBindings body'
        pure (Ready (L (exprSpan repaired) repaired))
      _ -> Left (UpdateError EPT.NotABindingContainer)

insertBindingText :: EPT.BindingInsertPosition -> Text -> Update Expr
insertBindingText position source =
  Update $ \focused -> do
    newBinding <- withUpdateError $ EPF.parseBinding source
    runUpdate (insertBinding position newBinding) focused

insertInheritKeyAt :: Int -> LAttrKey -> Update Binding
insertInheritKeyAt idx newKey =
  Update $ \focused ->
    case focused of
      L span' (NixInheritBinding ann scope keys) -> do
        _ <- withUpdateError $ normalizeBindingInsertIndex (EPT.InsertBindingAt idx) (length keys)
        let anchoredKey = translateFromTo (getLoc newKey) (inheritKeyInsertSpan ann scope keys idx) newKey
            shiftedTail = shiftAttrKeysRight (inheritKeyShiftColumns anchoredKey) (drop idx keys)
        Ready <$> withPrepareError (prepareEditedRoot (L span' (NixInheritBinding ann scope (take idx keys <> [anchoredKey] <> shiftedTail))))
      _ -> Left (UpdateError EPT.NotABindingContainer)

insertInheritKeyTextAt :: Int -> Text -> Update Binding
insertInheritKeyTextAt idx source =
  Update $ \focused -> do
    newKey <- withUpdateError $ EPF.parseAttrKey source
    runUpdate (insertInheritKeyAt idx newKey) focused

class EditableRoot root where
  prepareEditedRoot :: Located root -> Either EPT.ExactPrintError (Located root)

instance EditableRoot Expr where
  prepareEditedRoot (L _ value) = do
    repaired <- prepareExpr value
    pure (L (exprSpan repaired) repaired)

instance EditableRoot Binding where
  prepareEditedRoot (L _ value) = do
    repaired <- prepareBinding value
    pure (L (bindingSpan repaired) repaired)

instance EditableRoot AttrPath where
  prepareEditedRoot (L _ value) = do
    repaired <- prepareAttrPath value
    pure (L (attrPathSpan repaired) repaired)

instance EditableRoot FuncPat where
  prepareEditedRoot (L _ value) = do
    repaired <- prepareFuncPat value
    pure (L (funcPatBodySpan repaired) repaired)

editWith :: (EditableRoot root) => Selector root focus -> Update focus -> Located root -> Either EditError (Located root)
editWith selector update input = do
  Selected focus rebuild <- withSelectionError $ runSelector selector input
  focus' <- runUpdate update focus
  rebuilt <- rebuild focus'
  case rebuilt of
    Ready root' -> Right root'
    Raw root' -> withPrepareError $ prepareEditedRoot root'

prepareAttrPathNode :: Located AttrPath -> Either EditError (Located AttrPath)
prepareAttrPathNode = withPrepareError . prepareEditedRoot

editedValue :: Edited a -> Located a
editedValue = \case
  Raw x -> x
  Ready x -> x

staticAttrKeyText :: AttrKey -> Maybe Text
staticAttrKeyText = \case
  NixStaticAttrKey _ (L _ key) -> Just key
  _ -> Nothing

staticAttrPathText :: AttrPath -> Maybe [Text]
staticAttrPathText (NixAttrPath _ keys) = traverse (staticAttrKeyText . unLoc) keys

bindingDefinesPath :: [Text] -> Binding -> Bool
bindingDefinesPath path = \case
  NixNormalBinding _ attrPath _ -> staticAttrPathText (unLoc attrPath) == Just path
  NixInheritBinding _ _ keys ->
    case path of
      [key] -> key `elem` mapMaybe (staticAttrKeyText . unLoc) keys
      _ -> False

bindingDefinesKey :: Text -> Binding -> Bool
bindingDefinesKey key = bindingDefinesPath [key]

bindingChildren :: Located Expr -> Maybe [LBinding]
bindingChildren (L _ expr) =
  case expr of
    NixSet _ _ (L _ bindings) -> Just bindings
    NixLet _ (L _ bindings) _ -> Just bindings
    _ -> Nothing

rebuildBindingContainer :: [LBinding] -> Located Expr -> Either EditError (Edited Expr)
rebuildBindingContainer children (L _ expr) =
  case expr of
    NixSet ann kind _ -> do
      repaired <- withPrepareError $ rebuildSetLayout ann kind children
      pure (Ready (L (exprSpan repaired) repaired))
    NixLet ann _ body -> do
      repaired <- withPrepareError $ rebuildLetLayout ann children body
      pure (Ready (L (exprSpan repaired) repaired))
    _ -> expr `seq` error "impossible: binding rebuild on non-binding container"

findBindingIndex :: (Binding -> Bool) -> [LBinding] -> Maybe Int
findBindingIndex predicate = go 0
  where
    go _ [] = Nothing
    go idx (binding : rest)
      | predicate (unLoc binding) = Just idx
      | otherwise = go (idx + 1) rest

findInheritKeyIndex :: Text -> [LAttrKey] -> Maybe Int
findInheritKeyIndex key = go 0
  where
    go _ [] = Nothing
    go idx (attrKey : rest)
      | staticAttrKeyText (unLoc attrKey) == Just key = Just idx
      | otherwise = go (idx + 1) rest

inheritScopeReplacement :: LExpr -> LExpr -> LExpr
inheritScopeReplacement oldScope replacement =
  case unLoc oldScope of
    NixPar ann _ -> L (getLoc oldScope) (NixPar ann replacement)
    _ -> translateFromTo (getLoc replacement) (getLoc oldScope) replacement

inheritKeyInsertSpan :: AnnInheritBinding -> Maybe LExpr -> [LAttrKey] -> Int -> SrcSpan
inheritKeyInsertSpan ann scope keys idx =
  let anchor
        | idx > 0 = getLoc (keys !! (idx - 1))
        | otherwise = maybe (expectTokenSpan "inherit keyword" (aibInherit ann)) getLoc scope
   in mkSrcSpan (srcSpanFilename anchor) (srcSpanEndLine anchor, srcSpanEndColumn anchor + 1) (srcSpanEndLine anchor, srcSpanEndColumn anchor + 2)

inheritKeyShiftColumns :: LAttrKey -> Int
inheritKeyShiftColumns key = (srcSpanEndColumn span' - srcSpanStartColumn span') + 1
  where
    span' = getLoc key

shiftAttrKeysRight :: Int -> [LAttrKey] -> [LAttrKey]
shiftAttrKeysRight delta = fmap shiftOne
  where
    shiftOne key = translateFromTo (getLoc key) (shiftSpanRight delta (getLoc key)) key

shiftSpanRight :: Int -> SrcSpan -> SrcSpan
shiftSpanRight delta span' =
  mkSrcSpan
    (srcSpanFilename span')
    (srcSpanStartLine span', srcSpanStartColumn span' + delta)
    (srcSpanEndLine span', srcSpanEndColumn span' + delta)

prepareInsertedLetBody :: AnnLetNode -> [LBinding] -> LExpr -> LExpr
prepareInsertedLetBody ann bindings body
  | srcSpanStartLine span' > srcSpanEndLine oldInSpan = translateFromTo span' shifted body
  | otherwise = body
  where
    span' = getLoc body
    oldInSpan = expectTokenSpan "in keyword" (alIn ann)
    newInLine = case reverse bindings of
      lastBinding : _ -> srcSpanEndLine (getLoc lastBinding) + 1
      [] -> srcSpanStartLine oldInSpan
    shifted =
      mkSrcSpan
        (srcSpanFilename oldInSpan)
        (newInLine + 1, srcSpanStartColumn span')
        (newInLine + 1, srcSpanStartColumn span' + 1)

normalizeBindingInsertIndex :: EPT.BindingInsertPosition -> Int -> EPT.ExactPrintResult Int
normalizeBindingInsertIndex EPT.AppendBinding len = Right len
normalizeBindingInsertIndex (EPT.InsertBindingAt idx) len
  | idx < 0 = Left (EPT.NegativeIndex idx)
  | idx > len = Left (EPT.IndexOutOfRange idx len)
  | otherwise = Right idx

bindingInsertAnchor :: Int -> BindingSequenceAnchor
bindingInsertAnchor idx
  | idx == 0 = AnchorStartAtFirstSlot
  | otherwise = AnchorPreserveExisting

insertAt :: Int -> a -> [a] -> [a]
insertAt idx x xs = take idx xs <> [x] <> drop idx xs

editExpr :: Selector Expr focus -> Update focus -> LExpr -> Either EditError LExpr
editExpr = editWith

editBinding :: Selector Binding focus -> Update focus -> LBinding -> Either EditError LBinding
editBinding = editWith

editAttrPath :: Selector AttrPath focus -> Update focus -> LAttrPath -> Either EditError LAttrPath
editAttrPath = editWith

editFuncPat :: Selector FuncPat focus -> Update focus -> LFuncPat -> Either EditError LFuncPat
editFuncPat = editWith

mismatch :: (Data node) => Text -> Located node -> SelectError
mismatch name focused =
  SelectorMismatch
    { selectorName = name,
      actualConstructor = T.pack (showConstr (toConstr (unLoc focused))),
      selectionSpan = getLoc focused
    }

required :: (Data parent) => Text -> (Located parent -> Maybe (Selected parent child)) -> Selector parent child
required name f =
  Selector
    { runSelector = \parent ->
        case f parent of
          Just child -> Right child
          Nothing -> Left (mismatch name parent)
    }

optional :: (Data parent) => Text -> (Located parent -> Maybe (Maybe (Selected parent child))) -> Selector parent child
optional name f =
  Selector
    { runSelector = \parent ->
        case f parent of
          Just (Just child) -> Right child
          Just Nothing -> Left OptionalAbsent {selectorName = name, selectionSpan = getLoc parent}
          Nothing -> Left (mismatch name parent)
    }

indexed :: (Data parent, Data child) => Text -> Int -> (Located parent -> Maybe [Located child]) -> ([Located child] -> Located parent -> Either EditError (Edited parent)) -> Selector parent child
indexed name idx getChildren rebuildParent =
  Selector
    { runSelector = \parent ->
        case getChildren parent of
          Nothing -> Left (mismatch name parent)
          Just children
            | idx < 0 || idx >= length children ->
                Left
                  IndexOutOfBounds
                    { selectorName = name,
                      index = idx,
                      itemCount = length children,
                      selectionSpan = getLoc parent
                    }
            | otherwise ->
                let target = children !! idx
                 in Right
                      Selected
                        { selected = target,
                          replaceSelected = \replacement ->
                            let replacement' = translateFromTo (getLoc (editedValue replacement)) (getLoc target) (editedValue replacement)
                                updated = take idx children <> [replacement'] <> drop (idx + 1) children
                             in rebuildParent updated parent
                        }
    }

bindingMatching :: Text -> [Text] -> (Binding -> Bool) -> Selector Expr Binding
bindingMatching name query matches =
  Selector
    { runSelector = \parent ->
        case bindingChildren parent of
          Nothing -> Left (mismatch name parent)
          Just children ->
            case findBindingIndex matches children of
              Nothing -> Left BindingNotFound {selectorName = name, bindingQuery = query, selectionSpan = getLoc parent}
              Just idx ->
                let target = children !! idx
                 in Right
                      Selected
                        { selected = target,
                          replaceSelected = \replacement ->
                            let replacement' = translateFromTo (getLoc (editedValue replacement)) (getLoc target) (editedValue replacement)
                             in do
                                  replacement'' <- withPrepareError $ prepareEditedRoot replacement'
                                  let updated = take idx children <> [replacement''] <> drop (idx + 1) children
                                  rebuildBindingContainer updated parent
                        }
    }

$( makeRequiredSelectors
     [ RequiredSelectorSpec "letBody" "let body" [t|Expr|] [t|Expr|] 'NixLet 2,
       RequiredSelectorSpec "lambdaBody" "lambda body" [t|Expr|] [t|Expr|] 'NixLam 2,
       RequiredSelectorSpec "appFunction" "application function" [t|Expr|] [t|Expr|] 'NixApp 1,
       RequiredSelectorSpec "appArgument" "application argument" [t|Expr|] [t|Expr|] 'NixApp 2,
       RequiredSelectorSpec "ifCondition" "if condition" [t|Expr|] [t|Expr|] 'NixIf 1,
       RequiredSelectorSpec "ifThen" "if then" [t|Expr|] [t|Expr|] 'NixIf 2,
       RequiredSelectorSpec "ifElse" "if else" [t|Expr|] [t|Expr|] 'NixIf 3,
       RequiredSelectorSpec "withScope" "with scope" [t|Expr|] [t|Expr|] 'NixWith 1,
       RequiredSelectorSpec "withBody" "with body" [t|Expr|] [t|Expr|] 'NixWith 2,
       RequiredSelectorSpec "assertCondition" "assert condition" [t|Expr|] [t|Expr|] 'NixAssert 1,
       RequiredSelectorSpec "assertBody" "assert body" [t|Expr|] [t|Expr|] 'NixAssert 2,
       RequiredSelectorSpec "selectExpr" "select expression" [t|Expr|] [t|Expr|] 'NixSelect 1,
       RequiredSelectorSpec "hasAttrExpr" "has-attr expression" [t|Expr|] [t|Expr|] 'NixHasAttr 1
     ]
 )

$(makeRequiredSelectors [RequiredSelectorSpec "lambdaPattern" "lambda pattern" [t|Expr|] [t|FuncPat|] 'NixLam 1])

$( makeRequiredSelectors
     [ RequiredSelectorSpec "selectPath" "select path" [t|Expr|] [t|AttrPath|] 'NixSelect 2,
       RequiredSelectorSpec "hasAttrPath" "has-attr path" [t|Expr|] [t|AttrPath|] 'NixHasAttr 2
     ]
 )

$(makeOptionalSelectors [OptionalSelectorSpec "selectDefault" "select default" [t|Expr|] [t|Expr|] 'NixSelect 3])

$(makeRequiredSelectors [RequiredSelectorSpec "bindingPath" "binding path" [t|Binding|] [t|AttrPath|] 'NixNormalBinding 1])

$(makeRequiredSelectors [RequiredSelectorSpec "bindingValue" "binding value" [t|Binding|] [t|Expr|] 'NixNormalBinding 2])

$(makeRequiredSelectors [RequiredSelectorSpec "literal" "literal" [t|Expr|] [t|Lit|] 'NixLit 1])

bindingByKey :: Text -> Selector Expr Binding
bindingByKey key = bindingMatching "binding" [key] (bindingDefinesKey key)

bindingByPath :: [Text] -> Selector Expr Binding
bindingByPath path = bindingMatching "binding" path (bindingDefinesPath path)

bindingAt :: Int -> Selector Expr Binding
bindingAt idx =
  indexed "binding" idx bindingChildren rebuildBindingContainer

inheritScope :: Selector Binding Expr
inheritScope =
  optional "inherit scope" $ \binding ->
    case binding of
      L _ (NixInheritBinding _ Nothing _) -> Just Nothing
      L _ (NixInheritBinding ann (Just scope) keys) ->
        Just . Just $
          Selected
            { selected = scope,
              replaceSelected = \replacement ->
                let replacement' = inheritScopeReplacement scope (editedValue replacement)
                 in Ready <$> withPrepareError (prepareEditedRoot (L (getLoc binding) (NixInheritBinding ann (Just replacement') keys)))
            }
      _ -> Nothing

inheritKeyAt :: Int -> Selector Binding AttrKey
inheritKeyAt idx =
  indexed "inherit key" idx getChildren rebuildParent
  where
    getChildren (L _ (NixInheritBinding _ _ keys)) = Just keys
    getChildren _ = Nothing
    rebuildParent keys (L span' (NixInheritBinding ann scope _)) =
      Ready <$> withPrepareError (prepareEditedRoot (L span' (NixInheritBinding ann scope keys)))
    rebuildParent _ binding = binding `seq` error "impossible: inherit key rebuild on non-inherit binding"

inheritKey :: Text -> Selector Binding AttrKey
inheritKey key =
  Selector
    { runSelector = \binding ->
        case binding of
          L _ (NixInheritBinding ann scope keys) ->
            case findInheritKeyIndex key keys of
              Nothing -> Left BindingNotFound {selectorName = "inherit key", bindingQuery = [key], selectionSpan = getLoc binding}
              Just idx ->
                let target = keys !! idx
                 in Right
                      Selected
                        { selected = target,
                          replaceSelected = \replacement ->
                            let replacement' = translateFromTo (getLoc (editedValue replacement)) (getLoc target) (editedValue replacement)
                                updated = take idx keys <> [replacement'] <> drop (idx + 1) keys
                             in Ready <$> withPrepareError (prepareEditedRoot (L (getLoc binding) (NixInheritBinding ann scope updated)))
                        }
          _ -> Left (mismatch "inherit key" binding)
    }

listElement :: Int -> Selector Expr Expr
listElement idx =
  Selector
    { runSelector = \parent ->
        case parent of
          L _ (NixList ann children)
            | idx < 0 || idx >= length children ->
                Left
                  IndexOutOfBounds
                    { selectorName = "list element",
                      index = idx,
                      itemCount = length children,
                      selectionSpan = getLoc parent
                    }
            | otherwise ->
                let target = children !! idx
                 in Right
                      Selected
                        { selected = target,
                          replaceSelected = \replacement -> do
                            let replacement' = translateFromTo (getLoc (editedValue replacement)) (getLoc target) (editedValue replacement)
                            replacement'' <- withPrepareError $ prepareEditedRoot replacement'
                            repaired <- withPrepareError $ rebuildListLayoutWithAnchor AnchorStartAtFirstSlot ann (take idx children <> [replacement''] <> drop (idx + 1) children)
                            pure (Ready (L (exprSpan repaired) repaired))
                        }
          _ -> Left (mismatch "list element" parent)
    }

attrKeyAt :: Int -> Selector AttrPath AttrKey
attrKeyAt idx =
  indexed "attr key" idx getChildren rebuildParent
  where
    getChildren (L _ (NixAttrPath _ keys)) = Just keys
    rebuildParent keys (L span' (NixAttrPath ann _)) = Ready <$> prepareAttrPathNode (L span' (NixAttrPath ann keys))
