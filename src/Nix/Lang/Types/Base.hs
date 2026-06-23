{-# LANGUAGE DeriveDataTypeable #-}

module Nix.Lang.Types.Base
  ( Syn,
    Expr,
    Binding,
    AttrPath,
    AttrKey,
    FuncPat,
    Lit,
    StringPart,
    NString,
    Path,
    SetPatAs,
    SetPatBinding,
    mkVar,
    mkLit,
    mkPar,
    mkString,
    mkPath,
    mkEnvPath,
    mkLam,
    mkApp,
    mkBinApp,
    mkNot,
    mkNeg,
    mkList,
    mkSet,
    mkRecSet,
    mkLet,
    mkHasAttr,
    mkSelect,
    mkSelectOr,
    mkIf,
    mkWith,
    mkAssert,
    mkUriLit,
    mkIntegerLit,
    mkFloatLit,
    mkBooleanLit,
    mkNullLit,
    mkUri,
    mkInt,
    mkFloat,
    mkBool,
    mkTrue,
    mkFalse,
    mkNull,
    mkStringLiteral,
    mkInterpol,
    mkDoubleQuotedString,
    mkIndentedNString,
    mkText,
    mkIndentedText,
    mkLiteralPath,
    mkInterpolatedPath,
    mkAttr,
    mkQuotedAttr,
    mkDynamicAttr,
    mkInterpolatedAttr,
    mkAttrs,
    mkAttrPath,
    mkNormalBinding,
    mkBinding,
    mkBindingPath,
    mkInheritKeys,
    mkInherit,
    mkInheritFromKeys,
    mkInheritFrom,
    mkOptionalBinding,
    mkOptionalBindingPath,
    mkStaticSet,
    mkStaticRecSet,
    mkStaticLet,
    mkSelectAttrs,
    mkSelectOrAttrs,
    mkHasAttrs,
    mkVarPat,
    mkSetPat,
    mkSetPatBinding,
    mkSetPatAs
  ) where

import Data.Data (Data)
import Data.Text (Text)
import Nix.Lang.Types

data Syn deriving (Data)

type instance XRec Syn a = a

instance UnXRec Syn where
  unXRec _ = id

type instance NixVarName Syn = Text

type instance NixBinderName Syn = Text

type instance NixAttrName Syn = Text

type instance XNixUri Syn = NoExtF

type instance XNixInteger Syn = NoExtF

type instance XNixFloat Syn = NoExtF

type instance XNixBoolean Syn = NoExtF

type instance XNixNull Syn = NoExtF

type instance XXNixLit Syn = NoExtC

type instance XNixStringLiteral Syn = NoExtF

type instance XNixStringInterpol Syn = NoExtF

type instance XXNixStringPart Syn = NoExtC

type instance XNixDoubleQuotesString Syn = NoExtF

type instance XNixDoubleSingleQuotesString Syn = NoExtF

type instance XXNixString Syn = NoExtC

type instance XNixLiteralPath Syn = NoExtF

type instance XNixInterpolPath Syn = NoExtF

type instance XXNixPath Syn = NoExtC

type instance XNixStaticAttrKey Syn = NoExtF

type instance XNixDynamicStringAttrKey Syn = NoExtF

type instance XNixDynamicInterpolAttrKey Syn = NoExtF

type instance XXNixAttrKey Syn = NoExtC

type instance XNixAttrPath Syn = NoExtF

type instance XNixNormalBinding Syn = NoExtF

type instance XNixInheritBinding Syn = NoExtF

type instance XXNixBinding Syn = NoExtC

type instance XNixVarPat Syn = NoExtF

type instance XNixSetPat Syn = NoExtF

type instance XNixSetPatAs Syn = NoExtF

type instance XNixSetPatBinding Syn = NoExtF

type instance XXNixFuncPat Syn = NoExtC

type instance XNixVar Syn = NoExtF

type instance XNixLit Syn = NoExtF

type instance XNixPar Syn = NoExtF

type instance XNixString Syn = NoExtF

type instance XNixPath Syn = NoExtF

type instance XNixEnvPath Syn = NoExtF

type instance XNixLam Syn = NoExtF

type instance XNixApp Syn = NoExtF

type instance XNixBinApp Syn = NoExtF

type instance XNixNotApp Syn = NoExtF

type instance XNixNegApp Syn = NoExtF

type instance XNixList Syn = NoExtF

type instance XNixSet Syn = NoExtF

type instance XNixLet Syn = NoExtF

type instance XNixHasAttr Syn = NoExtF

type instance XNixSelect Syn = NoExtF

type instance XNixIf Syn = NoExtF

type instance XNixWith Syn = NoExtF

type instance XNixAssert Syn = NoExtF

type instance XXNixExpr Syn = NoExtC

type Expr = NixExpr Syn

type Binding = NixBinding Syn

type AttrPath = NixAttrPath Syn

type AttrKey = NixAttrKey Syn

type FuncPat = NixFuncPat Syn

type Lit = NixLit Syn

type StringPart = NixStringPart Syn

type NString = NixString Syn

type Path = NixPath Syn

type SetPatAs = NixSetPatAs Syn

type SetPatBinding = NixSetPatBinding Syn

mkVar :: Text -> Expr
mkVar = NixVar NoExtF

mkLit :: Lit -> Expr
mkLit = NixLit NoExtF

mkPar :: Expr -> Expr
mkPar = NixPar NoExtF

mkString :: NString -> Expr
mkString = NixString NoExtF

mkPath :: Path -> Expr
mkPath = NixPath NoExtF

mkEnvPath :: Text -> Expr
mkEnvPath = NixEnvPath NoExtF

mkLam :: FuncPat -> Expr -> Expr
mkLam = NixLam NoExtF

mkApp :: Expr -> Expr -> Expr
mkApp = NixApp NoExtF

mkBinApp :: BinaryOp -> Expr -> Expr -> Expr
mkBinApp = NixBinApp NoExtF

mkNot :: Expr -> Expr
mkNot = NixNotApp NoExtF

mkNeg :: Expr -> Expr
mkNeg = NixNegApp NoExtF

mkList :: [Expr] -> Expr
mkList = NixList NoExtF

mkSet :: [Binding] -> Expr
mkSet = NixSet NoExtF NixSetNonRecursive

mkRecSet :: [Binding] -> Expr
mkRecSet = NixSet NoExtF NixSetRecursive

mkLet :: [Binding] -> Expr -> Expr
mkLet = NixLet NoExtF

mkHasAttr :: Expr -> AttrPath -> Expr
mkHasAttr = NixHasAttr NoExtF

mkSelect :: Expr -> AttrPath -> Expr
mkSelect expr path' = NixSelect NoExtF expr path' Nothing

mkSelectOr :: Expr -> AttrPath -> Expr -> Expr
mkSelectOr expr path' fallback = NixSelect NoExtF expr path' (Just fallback)

mkIf :: Expr -> Expr -> Expr -> Expr
mkIf = NixIf NoExtF

mkWith :: Expr -> Expr -> Expr
mkWith = NixWith NoExtF

mkAssert :: Expr -> Expr -> Expr
mkAssert = NixAssert NoExtF

mkUriLit :: Text -> Lit
mkUriLit = NixUri NoExtF

mkIntegerLit :: Integer -> Lit
mkIntegerLit = NixInteger NoExtF

mkFloatLit :: Float -> Lit
mkFloatLit = NixFloat NoExtF

mkBooleanLit :: Bool -> Lit
mkBooleanLit = NixBoolean NoExtF

mkNullLit :: Lit
mkNullLit = NixNull NoExtF

mkUri :: Text -> Expr
mkUri = mkLit . mkUriLit

mkInt :: Integer -> Expr
mkInt = mkLit . mkIntegerLit

mkFloat :: Float -> Expr
mkFloat = mkLit . mkFloatLit

mkBool :: Bool -> Expr
mkBool = mkLit . mkBooleanLit

mkTrue :: Expr
mkTrue = mkBool True

mkFalse :: Expr
mkFalse = mkBool False

mkNull :: Expr
mkNull = mkLit mkNullLit

mkStringLiteral :: Text -> StringPart
mkStringLiteral = NixStringLiteral NoExtF

mkInterpol :: Expr -> StringPart
mkInterpol = NixStringInterpol NoExtF

mkDoubleQuotedString :: [StringPart] -> NString
mkDoubleQuotedString = NixDoubleQuotesString NoExtF

mkIndentedNString :: [StringPart] -> NString
mkIndentedNString = NixDoubleSingleQuotesString NoExtF

mkText :: Text -> Expr
mkText value = mkString (mkDoubleQuotedString [mkStringLiteral value])

mkIndentedText :: Text -> Expr
mkIndentedText value = mkString (mkIndentedNString [mkStringLiteral value])

mkLiteralPath :: Text -> Path
mkLiteralPath = NixLiteralPath NoExtF

mkInterpolatedPath :: [StringPart] -> Path
mkInterpolatedPath = NixInterpolPath NoExtF

mkAttr :: Text -> AttrKey
mkAttr = NixStaticAttrKey NoExtF

mkQuotedAttr :: Text -> AttrKey
mkQuotedAttr value = mkDynamicAttr [mkStringLiteral value]

mkDynamicAttr :: [StringPart] -> AttrKey
mkDynamicAttr = NixDynamicStringAttrKey NoExtF

mkInterpolatedAttr :: Expr -> AttrKey
mkInterpolatedAttr = NixDynamicInterpolAttrKey NoExtF

mkAttrs :: [Text] -> AttrPath
mkAttrs = mkAttrPath . fmap mkAttr

mkAttrPath :: [AttrKey] -> AttrPath
mkAttrPath = NixAttrPath NoExtF

mkNormalBinding :: AttrPath -> Expr -> Binding
mkNormalBinding = NixNormalBinding NoExtF

mkBinding :: [Text] -> Expr -> Binding
mkBinding pathParts value = mkBindingPath (fmap mkAttr pathParts) value

mkBindingPath :: [AttrKey] -> Expr -> Binding
mkBindingPath keys value = mkNormalBinding (mkAttrPath keys) value

mkInheritKeys :: [AttrKey] -> Binding
mkInheritKeys = mkInheritBinding Nothing

mkInherit :: [Text] -> Binding
mkInherit = mkInheritKeys . fmap mkAttr

mkInheritFromKeys :: Expr -> [AttrKey] -> Binding
mkInheritFromKeys scope = mkInheritBinding (Just scope)

mkInheritFrom :: Expr -> [Text] -> Binding
mkInheritFrom scope = mkInheritFromKeys scope . fmap mkAttr

mkOptionalBinding :: [Text] -> Maybe Expr -> [Binding]
mkOptionalBinding pathParts = maybe [] (pure . mkBinding pathParts)

mkOptionalBindingPath :: [AttrKey] -> Maybe Expr -> [Binding]
mkOptionalBindingPath keys = maybe [] (pure . mkBindingPath keys)

mkStaticSet :: [(Text, Expr)] -> Expr
mkStaticSet = mkSet . fmap (uncurry (mkBinding . pure))

mkStaticRecSet :: [(Text, Expr)] -> Expr
mkStaticRecSet = mkRecSet . fmap (uncurry (mkBinding . pure))

mkStaticLet :: [(Text, Expr)] -> Expr -> Expr
mkStaticLet bindings body = mkLet (fmap (uncurry (mkBinding . pure)) bindings) body

mkSelectAttrs :: Expr -> [Text] -> Expr
mkSelectAttrs expr = mkSelect expr . mkAttrs

mkSelectOrAttrs :: Expr -> [Text] -> Expr -> Expr
mkSelectOrAttrs expr pathParts fallback = mkSelectOr expr (mkAttrs pathParts) fallback

mkHasAttrs :: Expr -> [Text] -> Expr
mkHasAttrs expr = mkHasAttr expr . mkAttrs

mkVarPat :: Text -> FuncPat
mkVarPat = NixVarPat NoExtF

mkSetPat :: NixSetPatEllipses -> Maybe (NixSetPatAs Syn) -> [NixSetPatBinding Syn] -> FuncPat
mkSetPat = NixSetPat NoExtF

mkSetPatBinding :: Text -> Maybe Expr -> NixSetPatBinding Syn
mkSetPatBinding = NixSetPatBinding NoExtF

mkSetPatAs :: NixSetPatAsLocation -> Text -> NixSetPatAs Syn
mkSetPatAs location name = NixSetPatAs NoExtF location name

mkInheritBinding :: Maybe Expr -> [AttrKey] -> Binding
mkInheritBinding = NixInheritBinding NoExtF
