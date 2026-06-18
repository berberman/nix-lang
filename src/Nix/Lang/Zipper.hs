{-# LANGUAGE GADTs #-}

module Nix.Lang.Zipper where

import Control.Monad (guard)
import Control.Monad.State.Strict (MonadState (put), MonadTrans (lift), StateT, get, runStateT)
import Data.Generics (Data (toConstr), Typeable, cast, gmapM, gmapQr, showConstr)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.ExactPrint.Operations
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils

--------------------------------------------------------------------------------

data ZChild parent child where
  -- | Inner of paren
  Par :: ZChild Expr Expr
  -- | Function of an app
  AppF :: ZChild Expr Expr
  -- | Argument of an app
  AppA :: ZChild Expr Expr
  -- | RHS of a bin op
  BinR :: ZChild Expr Expr
  -- | LHS of a bin op
  BinL :: ZChild Expr Expr
  -- | Operand of a prefix op
  Prefix :: ZChild Expr Expr
  -- | Literal payload of a literal expression
  ExprLit :: ZChild Expr Lit
  -- | Pattern of a lambda
  LamP :: ZChild Expr FuncPat
  -- | Body of a lambda
  LamB :: ZChild Expr Expr
  -- | Body expression of a let expression
  LetB :: ZChild Expr Expr
  -- | Operand of @a ? b.c@ (@a@)
  HasAttrO :: ZChild Expr Expr
  -- | AttrPath of @a ? b.c@ (@b.c@)
  HasAttrP :: ZChild Expr AttrPath
  -- | Operand of @a.b.c@ (@a@)
  SelectO :: ZChild Expr Expr
  -- | AttrPath of @a.b.c@ (@b.c@)
  SelectP :: ZChild Expr AttrPath
  -- | Condition of an if
  IfC :: ZChild Expr Expr
  -- | Then of an if
  IfT :: ZChild Expr Expr
  -- | Else of an if
  IfE :: ZChild Expr Expr
  -- | Scope of a with
  WithS :: ZChild Expr Expr
  -- | Body of a with
  WithB :: ZChild Expr Expr
  -- | Condition of an assert
  AssertC :: ZChild Expr Expr
  -- | Body of an assert
  AssertB :: ZChild Expr Expr
  -- | Pattern of a normal binding
  BindingP :: ZChild Binding AttrPath
  -- | Value of a normal binding
  BindingV :: ZChild Binding Expr

data ZOptional parent child where
  -- | Default of @a.b or d@ (@d@)
  SelectD :: ZOptional Expr Expr
  -- | Scope of an @inherit (scope) ...@ (@scope@)
  InheritS :: ZOptional Binding Expr

-- Future:
-- SetPatternAs
-- SetPatternBindingDefault

data ZSeq parent child where
  -- | Bindings in an attr set
  SSet :: ZSeq Expr Binding
  -- | Bindings in a let body
  SLet :: ZSeq Expr Binding
  -- | Elements in a list
  SList :: ZSeq Expr Expr
  -- | Attr keys in an attr path
  SAttrPath :: ZSeq AttrPath AttrKey
  -- | Attr keys in inherit (see 'NixInheritBinding')
  SInherit :: ZSeq Binding AttrKey
  -- | Bindings in a set pattern
  SSetPat :: ZSeq FuncPat SetPatBinding
  -- | Parts in a string
  SString :: ZSeq NString (NixStringPart Ps)

--------------------------------------------------------------------------------

data ZRoute parent child where
  -- | Focus on a child of a node
  Required :: ZChild parent child -> ZRoute parent child
  -- | Focus on an optional child of a node
  Optional :: ZOptional parent child -> ZRoute parent child
  -- | Focus on an indexed slot of a node (it's a sequence)
  Item :: ZSeq parent child -> Int -> ZRoute parent child

data ZPath root focus where
  -- | Get the focus on root
  Top :: ZPath root root
  -- | Given a focus on parent in the context of root,
  --   the parent, and one-step focus from parent to child, focus on child
  Step :: (Data parent, Data child, Typeable child) => ZPath root parent -> Located parent -> ZRoute parent child -> ZPath root child

-- | The zipper
data Focus root focus = Focus
  { focus :: Located focus,
    path :: ZPath root focus
  }

rootFocus :: Located root -> Focus root root
rootFocus root = Focus root Top

--------------------------------------------------------------------------------

data ZipperError
  = RouteMismatch
      { mismatchRoute :: Text,
        actualConstructor :: Text,
        parentSpan :: SrcSpan
      }
  | RouteFieldMissing
      { missingRoute :: Text,
        missingOccurrence :: Int,
        parentSpan :: SrcSpan
      }
  | OptionalChildAbsent
      { absentRoute :: Text,
        parentSpan :: SrcSpan
      }
  | ItemIndexOutOfBounds
      { sequenceRoute :: Text,
        itemIndex :: Int,
        itemCount :: Int,
        parentSpan :: SrcSpan
      }
  deriving (Show, Eq)

type ZipperResult = Either ZipperError

--------------------------------------------------------------------------------

data FieldSpec = FieldSpec
  { fsLabel :: Text,
    fsCtors :: [String],
    fsOcc :: Int
  }

data SeqStorage = DirectList | LocatedList

data SeqSpec = SeqSpec
  { ssField :: FieldSpec,
    ssStorage :: SeqStorage
  }

-- | Change this if the AST changes
childSpec :: ZChild parent child -> FieldSpec
childSpec = \case
  Par -> FieldSpec "parenthesized inner" ["NixPar"] 0
  AppF -> FieldSpec "application function" ["NixApp"] 0
  AppA -> FieldSpec "application argument" ["NixApp"] 1
  BinL -> FieldSpec "binary left operand" ["NixBinApp"] 0
  BinR -> FieldSpec "binary right operand" ["NixBinApp"] 1
  Prefix -> FieldSpec "prefix operand" ["NixNotApp", "NixNegApp"] 0
  ExprLit -> FieldSpec "literal expression payload" ["NixLit"] 0
  LamP -> FieldSpec "lambda pattern" ["NixLam"] 0
  LamB -> FieldSpec "lambda body" ["NixLam"] 0
  LetB -> FieldSpec "let body" ["NixLet"] 0
  HasAttrO -> FieldSpec "has-attr operand" ["NixHasAttr"] 0
  HasAttrP -> FieldSpec "has-attr path" ["NixHasAttr"] 0
  SelectO -> FieldSpec "selection operand" ["NixSelect"] 0
  SelectP -> FieldSpec "selection path" ["NixSelect"] 0
  IfC -> FieldSpec "if condition" ["NixIf"] 0
  IfT -> FieldSpec "if then branch" ["NixIf"] 1
  IfE -> FieldSpec "if else branch" ["NixIf"] 2
  WithS -> FieldSpec "with scope" ["NixWith"] 0
  WithB -> FieldSpec "with body" ["NixWith"] 1
  AssertC -> FieldSpec "assert condition" ["NixAssert"] 0
  AssertB -> FieldSpec "assert body" ["NixAssert"] 1
  BindingP -> FieldSpec "normal-binding path" ["NixNormalBinding"] 0
  BindingV -> FieldSpec "normal-binding value" ["NixNormalBinding"] 0

-- | Change this if the AST changes
optionalSpec :: ZOptional parent child -> FieldSpec
optionalSpec = \case
  SelectD -> FieldSpec "selection default" ["NixSelect"] 0
  InheritS -> FieldSpec "inherit scope" ["NixInheritBinding"] 0

-- | Change this if the AST changes
seqSpec :: ZSeq parent child -> SeqSpec
seqSpec = \case
  SSet -> located "set bindings" ["NixSet"] 0
  SLet -> located "let bindings" ["NixLet"] 0
  SList -> direct "list elements" ["NixList"] 0
  SAttrPath -> direct "attribute-path keys" ["NixAttrPath"] 0
  SInherit -> direct "inherit names" ["NixInheritBinding"] 0
  SSetPat -> direct "set-pattern bindings" ["NixSetPat"] 0
  SString -> direct "string parts" ["NixDoubleQuotesString", "NixDoubleSingleQuotesString"] 0
  where
    direct label constructors occurrence =
      SeqSpec (FieldSpec label constructors occurrence) DirectList
    located label constructors occurrence =
      SeqSpec (FieldSpec label constructors occurrence) LocatedList

routeLabel :: ZRoute parent child -> Text
routeLabel = \case
  Required child -> fsLabel $ childSpec child
  Optional child -> fsLabel $ optionalSpec child
  Item s index -> fsLabel (ssField (seqSpec s)) <> "[" <> T.pack (show index) <> "]"

--------------------------------------------------------------------------------

ctor :: (Data a) => a -> String
ctor = showConstr . toConstr

matchCtor :: (Data a) => FieldSpec -> a -> Bool
matchCtor FieldSpec {..} v = ctor v `elem` fsCtors

-- | Immediate fields of exactly the requested type.
immediateFields :: forall field parent. (Data parent, Typeable field) => parent -> [field]
immediateFields = gmapQr (<>) [] (\field -> maybe [] pure (cast field))

-- | Replace the @n@th immediate field of exactly the requested type.
replaceNthField ::
  forall field parent.
  (Data parent, Typeable field) =>
  Int ->
  field ->
  parent ->
  Maybe parent
replaceNthField wanted replacement parent = do
  (parent', (_, replaced)) <- runStateT (gmapM step parent) (0 :: Int, False)
  guard replaced
  pure parent'
  where
    step :: forall a. (Data a) => a -> StateT (Int, Bool) Maybe a
    step current =
      case cast current :: Maybe field of
        Nothing -> pure current
        Just _ -> do
          (seen, replaced) <- get
          let isWanted = seen == wanted
          put (seen + 1, replaced || isWanted)
          if isWanted
            then case cast replacement of
              Just replacement' -> pure replacement'
              Nothing -> lift Nothing
            else pure current

getField ::
  forall field parent.
  (Data parent, Typeable field) =>
  FieldSpec ->
  Located parent ->
  ZipperResult field
getField fieldSpec@FieldSpec {..} parent@(L _ node)
  | not (matchCtor fieldSpec node) =
      Left
        RouteMismatch
          { mismatchRoute = fsLabel,
            actualConstructor = T.pack $ ctor node,
            parentSpan = getLoc parent
          }
  | otherwise =
      case drop fsOcc (immediateFields @field node) of
        field : _ -> Right field
        [] ->
          Left
            RouteFieldMissing
              { missingRoute = fsLabel,
                missingOccurrence = fsOcc,
                parentSpan = getLoc parent
              }

putField ::
  forall field parent.
  (Data parent, Typeable field) =>
  FieldSpec ->
  field ->
  Located parent ->
  ZipperResult (Located parent)
putField fieldSpec@FieldSpec {..} replacement (L span' node)
  | not (matchCtor fieldSpec node) =
      Left
        RouteMismatch
          { mismatchRoute = fsLabel,
            actualConstructor = T.pack $ ctor node,
            parentSpan = span'
          }
  | otherwise =
      case replaceNthField @field fsOcc replacement node of
        Just node' -> Right (L span' node')
        Nothing ->
          Left
            RouteFieldMissing
              { missingRoute = fsLabel,
                missingOccurrence = fsOcc,
                parentSpan = span'
              }

--------------------------------------------------------------------------------

getSeq ::
  forall parent child.
  (Data parent, Typeable child) =>
  ZSeq parent child ->
  Located parent ->
  ZipperResult [Located child]
getSeq s parent =
  case seqSpec s of
    SeqSpec field DirectList ->
      getField @[Located child] field parent
    SeqSpec field LocatedList ->
      unLoc <$> getField @(Located [Located child]) field parent

putSeq ::
  forall parent child.
  (Data parent, Typeable child) =>
  ZSeq parent child ->
  [Located child] ->
  Located parent ->
  ZipperResult (Located parent)
putSeq s children parent =
  case seqSpec s of
    SeqSpec field DirectList ->
      putField @[Located child] field children parent
    SeqSpec field LocatedList -> do
      old <- getField @(Located [Located child]) field parent
      putField @(Located [Located child]) field (L (getLoc old) children) parent

getRoute ::
  forall parent child.
  (Data parent, Typeable child) =>
  ZRoute parent child ->
  Located parent ->
  ZipperResult (Located child)
getRoute = \case
  Required child -> getField @(Located child) (childSpec child)
  Optional child -> \parent -> do
    value <- getField @(Maybe (Located child)) (optionalSpec child) parent
    case value of
      Just focused -> Right focused
      Nothing ->
        Left
          OptionalChildAbsent
            { absentRoute = fsLabel (optionalSpec child),
              parentSpan = getLoc parent
            }
  Item s index -> \parent -> do
    children <- getSeq s parent
    if index < 0 || index >= length children
      then
        Left
          ItemIndexOutOfBounds
            { sequenceRoute = fsLabel (ssField (seqSpec s)),
              itemIndex = index,
              itemCount = length children,
              parentSpan = getLoc parent
            }
      else Right (children !! index)

putRoute ::
  forall parent child.
  (Data parent, Typeable child) =>
  ZRoute parent child ->
  Located child ->
  Located parent ->
  ZipperResult (Located parent)
putRoute route replacement parent =
  case route of
    Required child ->
      putField @(Located child)
        (childSpec child)
        replacement
        parent
    Optional child ->
      putField @(Maybe (Located child))
        (optionalSpec child)
        (Just replacement)
        parent
    Item s index -> do
      children <- getSeq s parent
      if index < 0 || index >= length children
        then
          Left
            ItemIndexOutOfBounds
              { sequenceRoute = fsLabel (ssField (seqSpec s)),
                itemIndex = index,
                itemCount = length children,
                parentSpan = getLoc parent
              }
        else
          putSeq
            s
            (take index children <> [replacement] <> drop (index + 1) children)
            parent

--------------------------------------------------------------------------------

down ::
  (Data parent, Data child) =>
  ZRoute parent child ->
  Focus root parent ->
  ZipperResult (Focus root child)
down route Focus {..} = do
  child <- getRoute route focus
  pure
    Focus
      { focus = child,
        path = Step path focus route
      }

downChild ::
  (Data parent, Data child) =>
  ZChild parent child ->
  Focus root parent ->
  Either ZipperError (Focus root child)
downChild = down . Required

downOptional ::
  (Data parent, Data child) =>
  ZOptional parent child ->
  Focus root parent ->
  Either ZipperError (Focus root child)
downOptional = down . Optional

downItem ::
  (Data parent, Data child) =>
  ZSeq parent child ->
  Int ->
  Focus root parent ->
  Either ZipperError (Focus root child)
downItem s idx = down $ Item s idx

replaceFocus :: Located focus -> Focus root focus -> Focus root focus
replaceFocus replacement focused = focused {focus = replacement}

modifyFocus :: (Located focus -> Located focus) -> Focus root focus -> Focus root focus
modifyFocus f focused = focused {focus = f $ focus focused}

replaceChildAt :: (Data parent, Data child) => ZRoute parent child -> Located child -> Focus root parent -> ZipperResult (Focus root parent)
replaceChildAt route replacement (Focus parent parentPath) = do
  parent' <- putRoute route replacement parent
  pure (Focus parent' parentPath)

plug :: ZPath root focus -> Located focus -> ZipperResult (Located root)
plug Top node = Right $ node
plug (Step parentPath parent route) child = do
  parent' <- putRoute route child parent
  plug parentPath parent'

closeRaw :: Focus root focus -> ZipperResult (Located root)
closeRaw Focus {..} = plug path focus

--------------------------------------------------------------------------------
