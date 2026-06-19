{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix.Lang.Edit
  ( Selector,
    Update,
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
    listElement,
    attrKeyAt,
    bindingPath,
    bindingValue,
    literal,
    replace,
    modify,
    modifyLocated,
    replaceExprText,
    replaceBindingText,
    replaceAttrKeyText,
    setIntLiteral,
    renameAttrKey,
    editExpr,
    editBinding,
    editAttrPath,
    editFuncPat,
  )
where

import Data.Bifunctor (first)
import Data.Data (Data, toConstr)
import Data.Generics (showConstr)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Edit.Internal.TH
import qualified Nix.Lang.ExactPrint.Edit.Fragment as EPF
import Nix.Lang.ExactPrint.Edit.Geometry (bindingSpan, translateFromTo)
import Nix.Lang.ExactPrint.Edit.Rebuild (BindingSequenceAnchor (AnchorStartAtFirstSlot), rebuildLetLayout, rebuildListLayoutWithAnchor, rebuildSetLayout)
import qualified Nix.Lang.ExactPrint.Edit.Types as EPT
import Nix.Lang.ExactPrint.Finalize (finalizeAttrPath, finalizeBinding, finalizeExpr, finalizeFuncPat, noLayoutHints)
import Nix.Lang.ExactPrint.Operations (attrPathSpan, exprSpan, funcPatBodySpan)
import Nix.Lang.Span
import Nix.Lang.Types
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
  deriving (Show, Eq)

data EditError
  = SelectionError SelectError
  | UpdateError Text
  | FinalizeError Text
  | ExpectedIntegerLiteral SrcSpan
  deriving (Show, Eq)

data Selected parent child = Selected
  { selected :: Located child,
    replaceSelected :: Located child -> Either EditError (Located parent)
  }

data Selector parent child = Selector
  { runSelector :: Located parent -> Either SelectError (Selected parent child),
    selectorNeedsFinalization :: Bool
  }

newtype Update focus = Update
  { runUpdate :: Located focus -> Either EditError (Located focus)
  }

root :: Selector a a
root = Selector {runSelector = \x -> Right Selected {selected = x, replaceSelected = Right}, selectorNeedsFinalization = True}

infixl 5 //

(//) :: Selector a b -> Selector b c -> Selector a c
Selector ab _ // Selector bc _ =
  Selector
    { runSelector = \a -> do
        Selected b putB <- ab a
        Selected c putC <- bc b
        pure
          Selected
            { selected = c,
              replaceSelected = \c' -> putC c' >>= putB
            },
      selectorNeedsFinalization = False
    }

replace :: Located focus -> Update focus
replace replacement = Update (Right . const replacement)

modify :: (focus -> focus) -> Update focus
modify f = modifyLocated (fmap f)

modifyLocated :: (Located focus -> Located focus) -> Update focus
modifyLocated f = Update (Right . f)

replaceExprText :: Text -> Update Expr
replaceExprText source = Update $ \focused -> do
  replacement <- first (UpdateError . showInternalError) $ EPF.parseExpr source
  pure (translateFromTo (getLoc replacement) (getLoc focused) replacement)

replaceBindingText :: Text -> Update Binding
replaceBindingText source = Update $ \focused -> do
  replacement <- first (UpdateError . showInternalError) $ EPF.parseBinding source
  pure (translateFromTo (getLoc replacement) (getLoc focused) replacement)

replaceAttrKeyText :: Text -> Update AttrKey
replaceAttrKeyText source = Update $ \focused -> do
  replacement <- first (UpdateError . showInternalError) $ EPF.parseAttrKey source
  pure (translateFromTo (getLoc replacement) (getLoc focused) replacement)

setIntLiteral :: Integer -> Update Expr
setIntLiteral value = Update $ \focused ->
  case unLoc focused of
    NixLit ann (L litSpan (NixInteger _ _)) ->
      Right (L (getLoc focused) (NixLit ann (L litSpan (NixInteger NoExtF value))))
    _ -> Left $ ExpectedIntegerLiteral (getLoc focused)

renameAttrKey :: Text -> Update AttrKey
renameAttrKey = replaceAttrKeyText

class EditableRoot root where
  finalizeEditedRoot :: Located root -> Either Text (Located root)

instance EditableRoot Expr where
  finalizeEditedRoot (L _ value) = do
    repaired <- first showInternalError $ finalizeExpr noLayoutHints value
    pure (L (exprSpan repaired) repaired)

instance EditableRoot Binding where
  finalizeEditedRoot (L _ value) = do
    repaired <- first showInternalError $ finalizeBinding noLayoutHints value
    pure (L (bindingSpan repaired) repaired)

instance EditableRoot AttrPath where
  finalizeEditedRoot (L _ value) = do
    repaired <- first showInternalError $ finalizeAttrPath noLayoutHints value
    pure (L (attrPathSpan repaired) repaired)

instance EditableRoot FuncPat where
  finalizeEditedRoot (L _ value) = do
    repaired <- first showInternalError $ finalizeFuncPat noLayoutHints value
    pure (L (funcPatBodySpan repaired) repaired)

editWith :: (EditableRoot root) => Selector root focus -> Update focus -> Located root -> Either EditError (Located root)
editWith selector update input = do
  Selected focus rebuild <- first SelectionError $ runSelector selector input
  focus' <- runUpdate update focus
  rawRoot <- rebuild focus'
  if selectorNeedsFinalization selector
    then first FinalizeError $ finalizeEditedRoot rawRoot
    else Right rawRoot

finalizeAttrPathNode :: Located AttrPath -> Either EditError (Located AttrPath)
finalizeAttrPathNode = first FinalizeError . finalizeEditedRoot

showInternalError :: EPT.EditError -> Text
showInternalError = T.pack . show

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
          Nothing -> Left (mismatch name parent),
      selectorNeedsFinalization = False
    }

optional :: (Data parent) => Text -> (Located parent -> Maybe (Maybe (Selected parent child))) -> Selector parent child
optional name f =
  Selector
    { runSelector = \parent ->
        case f parent of
          Just (Just child) -> Right child
          Just Nothing -> Left OptionalAbsent {selectorName = name, selectionSpan = getLoc parent}
          Nothing -> Left (mismatch name parent),
      selectorNeedsFinalization = False
    }

indexed :: (Data parent, Data child) => Text -> Int -> (Located parent -> Maybe [Located child]) -> ([Located child] -> Located parent -> Either EditError (Located parent)) -> Selector parent child
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
                            let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
                                updated = take idx children <> [replacement'] <> drop (idx + 1) children
                             in rebuildParent updated parent
                        },
      selectorNeedsFinalization = False
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

bindingAt :: Int -> Selector Expr Binding
bindingAt idx =
  indexed "binding" idx getChildren rebuildParent
  where
    getChildren (L _ expr) =
      case expr of
        NixSet _ _ (L _ bindings) -> Just bindings
        NixLet _ (L _ bindings) _ -> Just bindings
        _ -> Nothing
    rebuildParent children (L _ expr) =
      case expr of
        NixSet ann kind _ -> do
          repaired <- first (FinalizeError . showInternalError) $ rebuildSetLayout ann kind children
          pure (L (exprSpan repaired) repaired)
        NixLet ann _ body -> do
          repaired <- first (FinalizeError . showInternalError) $ rebuildLetLayout ann children body
          pure (L (exprSpan repaired) repaired)
        _ -> expr `seq` error "impossible: binding rebuild on non-binding container"

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
                            let replacement' = translateFromTo (getLoc replacement) (getLoc target) replacement
                            replacement'' <- first FinalizeError $ finalizeEditedRoot replacement'
                            repaired <- first (FinalizeError . showInternalError) $ rebuildListLayoutWithAnchor AnchorStartAtFirstSlot ann (take idx children <> [replacement''] <> drop (idx + 1) children)
                            pure (L (exprSpan repaired) repaired)
                        }
          _ -> Left (mismatch "list element" parent),
      selectorNeedsFinalization = False
    }

attrKeyAt :: Int -> Selector AttrPath AttrKey
attrKeyAt idx =
  indexed "attr key" idx getChildren rebuildParent
  where
    getChildren (L _ (NixAttrPath _ keys)) = Just keys
    rebuildParent keys (L span' (NixAttrPath ann _)) = finalizeAttrPathNode (L span' (NixAttrPath ann keys))
