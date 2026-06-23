{-# LANGUAGE DeriveDataTypeable #-}

module Nix.Lang.Types.Parsed where

import Data.Data (Data)
import Data.Text (Text)
import Nix.Lang.Annotation
import Nix.Lang.Span (Located (..))
import Nix.Lang.Types

data Ps deriving (Data)

type instance XRec Ps a = Located a

instance UnXRec Ps where
  unXRec _ (L _ x) = x

newtype SourceText = SourceText Text
  deriving (Eq, Show, Ord, Data)

type instance NixVarName Ps = Text

type instance NixBinderName Ps = Text

type instance NixAttrName Ps = Text

type instance XNixUri Ps = NoExtF

type instance XNixInteger Ps = NoExtF

type instance XNixFloat Ps = NoExtF

type instance XNixBoolean Ps = NoExtF

type instance XNixNull Ps = NoExtF

type instance XNixStringLiteral Ps = NoExtF

type instance XNixStringInterpol Ps = NoExtF

type instance XXNixStringPart Ps = NoExtC

type instance XNixDoubleQuotesString Ps = SourceText

type instance XNixDoubleSingleQuotesString Ps = SourceText

type instance XXNixString Ps = NoExtC

type instance XNixLiteralPath Ps = NoExtF

type instance XNixInterpolPath Ps = NoExtF

type instance XXNixPath Ps = NoExtC

type instance XNixStaticAttrKey Ps = NoExtF

type instance XNixDynamicStringAttrKey Ps = NoExtF

type instance XNixDynamicInterpolAttrKey Ps = NoExtF

type instance XXNixAttrKey Ps = NoExtC

type instance XNixAttrPath Ps = AnnAttrPath

type instance XNixNormalBinding Ps = AnnNormalBinding

type instance XNixInheritBinding Ps = AnnInheritBinding

type instance XXNixBinding Ps = NoExtC

type instance XNixVarPat Ps = AnnVarPat

type instance XNixSetPat Ps = AnnSetPatNode

type instance XNixSetPatAs Ps = AnnSetPatAs

type instance XNixSetPatBinding Ps = AnnSetPatBinding

type instance XXNixFuncPat Ps = NoExtC

type instance XNixVar Ps = AnnCommon

type instance XNixLit Ps = AnnCommon

type instance XNixPar Ps = AnnParNode

type instance XXNixLit Ps = NoExtC

type instance XNixString Ps = AnnStringNode

type instance XNixPath Ps = AnnPathNode

type instance XNixEnvPath Ps = AnnEnvPathNode

type instance XNixLam Ps = AnnLamNode

type instance XNixApp Ps = AnnAppNode

type instance XNixBinApp Ps = AnnBinAppNode

type instance XNixNotApp Ps = AnnPrefixNode

type instance XNixNegApp Ps = AnnPrefixNode

type instance XNixList Ps = AnnListNode

type instance XNixSet Ps = AnnSet

type instance XNixLet Ps = AnnLetNode

type instance XNixHasAttr Ps = AnnHasAttr

type instance XNixSelect Ps = AnnSelect

type instance XNixIf Ps = AnnIfNode

type instance XNixWith Ps = AnnWithNode

type instance XNixAssert Ps = AnnAssertNode

type instance XXNixExpr Ps = NoExtC

type Expr = NixExpr Ps

type LExpr = LNixExpr Ps

type Binding = NixBinding Ps

type LBinding = LNixBinding Ps

type AttrPath = NixAttrPath Ps

type LAttrPath = LNixAttrPath Ps

type AttrKey = NixAttrKey Ps

type LAttrKey = LNixAttrKey Ps

type FuncPat = NixFuncPat Ps

type LFuncPat = LNixFuncPat Ps

type Lit = NixLit Ps

type LLit = LNixLit Ps

type SetPatBinding = NixSetPatBinding Ps

type LSetPatBinding = Located SetPatBinding

type SetPatAs = NixSetPatAs Ps

type LSetPatAs = Located SetPatAs

type BinderName = NixBinderName Ps

type LBinderName = Located BinderName

type VarName = NixVarName Ps

type LVarName = Located VarName

type Path = NixPath Ps

type LPath = Located Path

type NString = NixString Ps

type LNString = Located NString
