{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Nix.Lang.Types where

import Data.Data (Data)
import Data.Text (Text)
import Nix.Lang.Annotation
import Nix.Lang.Span

--------------------------------------------------------------------------------
data Ps deriving (Data)

newtype SourceText = SourceText Text
  deriving (Eq, Show, Ord, Data)

data NoExtF = NoExtF
  deriving (Eq, Show, Data)

data NoExtC
  deriving (Data)

instance Show NoExtC where
  show _ = undefined

type instance NixId Ps = Text

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

--------------------------------------------------------------------------------

type Expr = NixExpr Ps

type LExpr = Located Expr

type Binding = NixBinding Ps

type LBinding = Located Binding

type AttrPath = NixAttrPath Ps

type LAttrPath = Located AttrPath

type AttrKey = NixAttrKey Ps

type LAttrKey = Located AttrKey

type FuncPat = NixFuncPat Ps

type LFuncPat = Located FuncPat

type Lit = NixLit Ps

type LLit = Located Lit

--------------------------------------------------------------------------------
data BinaryOp
  = -- | @++@
    OpConcat
  | -- | @*@
    OpMul
  | -- | @/@
    OpDiv
  | -- | @+@
    OpAdd
  | -- | @-@
    OpSub
  | -- | @//@
    OpUpdate
  | -- | @<@
    OpLT
  | -- | @<=@
    OpLE
  | -- | @>@
    OpGT
  | -- | @>=@
    OpGE
  | -- | @==@
    OpEq
  | -- | @!=@
    OpNEq
  | -- | @&&@
    OpAnd
  | -- | @||@
    OpOr
  | -- | @->@
    OpImpl
  deriving (Show, Eq, Enum, Data)

--------------------------------------------------------------------------------
data NixLit p
  = -- | @https://nixos.org/@
    NixUri (XNixUri p) Text
  | -- | @233@
    NixInteger (XNixInteger p) Integer
  | -- | @233.3@
    -- Note: Currently scientific and number started with decimal dot are not supported
    NixFloat (XNixFloat p) Float
  | -- | @true@ or @false@
    NixBoolean (XNixBoolean p) Bool
  | -- |  @null@
    NixNull (XNixNull p)
  | XNixLit !(XXNixLit p)

instance Eq (NixLit p) where
  (NixUri _ x) == (NixUri _ y) = x == y
  (NixInteger _ x) == (NixInteger _ y) = x == y
  (NixFloat _ x) == (NixFloat _ y) = x == y
  (NixBoolean _ x) == (NixBoolean _ y) = x == y
  (NixNull _) == (NixNull _) = True
  _ == _ = False

deriving instance
  ( Data p,
    Data (XNixUri p),
    Data (XNixInteger p),
    Data (XNixFloat p),
    Data (XNixNull p),
    Data (XNixBoolean p),
    Data (XXNixLit p)
  ) =>
  Data (NixLit p)

deriving instance
  ( Show (XNixUri p),
    Show (XNixInteger p),
    Show (XNixFloat p),
    Show (XNixNull p),
    Show (XNixBoolean p),
    Show (XXNixLit p)
  ) =>
  Show (NixLit p)

type LNixLit p = Located (NixLit p)

type family XNixUri p

type family XNixInteger p

type family XNixFloat p

type family XNixBoolean p

type family XNixNull p

type family XXNixLit p

--------------------------------------------------------------------------------

data NixStringPart p
  = -- | @x@
    -- As there is no annotation attached to this node, the location of 'Text'
    -- should be the same as the parent node; thus 'Text' doesn't need to have location
    -- information
    NixStringLiteral (XNixStringLiteral p) Text
  | -- | @${e}@, where e is a string
    NixStringInterpol (XNixStringInterpol p) (LNixExpr p)
  | XNixStringPart !(XXNixStringPart p)

deriving instance
  ( Data p,
    Data (XNixStringLiteral p),
    Data (XNixStringInterpol p),
    Data (XXNixStringPart p),
    Data (LNixExpr p)
  ) =>
  Data (NixStringPart p)

deriving instance
  ( Show (XNixStringLiteral p),
    Show (XNixStringInterpol p),
    Show (XXNixStringPart p),
    Show (LNixExpr p)
  ) =>
  Show (NixStringPart p)

type LNixStringPart p = Located (NixStringPart p)

type family XNixStringLiteral p

type family XNixStringInterpol p

type family XXNixStringPart p

--------------------------------------------------------------------------------

data NixString p
  = -- | @"x${e}x"@
    NixDoubleQuotesString (XNixDoubleQuotesString p) [LNixStringPart p]
  | -- | @
    -- '' A
    --    B ${c}
    --    d
    -- ''
    -- @
    -- In order to keep original source location correct, the parser won't strip
    -- common minimum indentation and do further processing.
    NixDoubleSingleQuotesString (XNixDoubleSingleQuotesString p) [LNixStringPart p]
  | XNixString !(XXNixString p)

deriving instance
  ( Data p,
    Data (XNixDoubleQuotesString p),
    Data (XNixDoubleSingleQuotesString p),
    Data (XXNixString p),
    Data (NixStringPart p)
  ) =>
  Data (NixString p)

deriving instance
  ( Show (XNixDoubleQuotesString p),
    Show (XNixDoubleSingleQuotesString p),
    Show (XXNixString p),
    Show (NixStringPart p)
  ) =>
  Show (NixString p)

type LNixString p = Located (NixString p)

type family XNixDoubleQuotesString p

type family XNixDoubleSingleQuotesString p

type family XXNixString p

--------------------------------------------------------------------------------
data NixPath p
  = -- | @./a/b/c@, @/a/b/c@, @~/a/b/c@
    --
    -- No consecutive slashes are allowed.
    NixLiteralPath (XNixLiteralPath p) Text
  | -- | @./${e}/b/c@, @./${a}-${b}/c/d${e}@
    --
    -- Since nix 2.13, slashes are no longer required between parts, but the behavior seems to be a bit strange:
    -- no more than two consecutive slashes can appear before the interpolation,
    -- while arbitrary number of slashes can appear after the interpolation.
    -- The parser won't enforce this.
    NixInterpolPath (XNixInterpolPath p) [LNixStringPart p]
  | XNixPath !(XXNixPath p)

deriving instance
  ( Data p,
    Data (XNixLiteralPath p),
    Data (XNixInterpolPath p),
    Data (LNixStringPart p),
    Data (XXNixPath p)
  ) =>
  Data (NixPath p)

deriving instance
  ( Show (XNixLiteralPath p),
    Show (XNixInterpolPath p),
    Show (LNixStringPart p),
    Show (XXNixPath p)
  ) =>
  Show (NixPath p)

type LNixPath p = Located (NixPath p)

type family XNixLiteralPath p

type family XNixInterpolPath p

type family XXNixPath p

--------------------------------------------------------------------------------
data NixAttrKey p
  = -- | @{ x = 123; }.x@
    NixStaticAttrKey (XNixStaticAttrKey p) (LNixId p)
  | -- | @{ x = 123; }."x"@, @{ x = 123; }."${"x"}"@
    -- Only double quoted strings are allowed
    NixDynamicStringAttrKey (XNixDynamicStringAttrKey p) [LNixStringPart p]
  | -- | @{ x = 123; }.${"x"}@
    NixDynamicInterpolAttrKey (XNixDynamicInterpolAttrKey p) (LNixExpr p)
  | XNixNixAttrKey !(XXNixAttrKey p)

deriving instance
  ( Data p,
    Data (XNixStaticAttrKey p),
    Data (XNixDynamicStringAttrKey p),
    Data (XNixDynamicInterpolAttrKey p),
    Data (LNixStringPart p),
    Data (XXNixAttrKey p),
    Data (NixId p),
    Data (NixExpr p)
  ) =>
  Data (NixAttrKey p)

deriving instance
  ( Show (XNixStaticAttrKey p),
    Show (XNixDynamicStringAttrKey p),
    Show (XNixDynamicInterpolAttrKey p),
    Show (LNixStringPart p),
    Show (XXNixAttrKey p),
    Show (NixId p),
    Show (NixExpr p)
  ) =>
  Show (NixAttrKey p)

type LNixAttrKey p = Located (NixAttrKey p)

type family XNixStaticAttrKey p

type family XNixDynamicStringAttrKey p

type family XNixDynamicInterpolAttrKey p

type family XXNixAttrKey p

--------------------------------------------------------------------------------

-- | @a.b.${c}."d"."${"e"}"@
data NixAttrPath p = NixAttrPath (XNixAttrPath p) [LNixAttrKey p]

deriving instance (Data p, Data (XNixAttrPath p), Data (NixAttrKey p)) => Data (NixAttrPath p)

deriving instance (Show (XNixAttrPath p), Show (NixAttrKey p)) => Show (NixAttrPath p)

type LNixAttrPath p = Located (NixAttrPath p)

type family XNixAttrPath p

--------------------------------------------------------------------------------

data NixBinding p
  = -- | @x = e;@
    NixNormalBinding (XNixNormalBinding p) (LNixAttrPath p) (LNixExpr p)
  | -- | @inherit a@, @inherit (a) b c@, @inherit (a) "b" ${"c"}@
    -- Note: Dynamic keys like @${"a" + "b"}@ are allowed since nix 2.13
    NixInheritBinding (XNixInheritBinding p) (Maybe (LNixExpr p)) [LNixAttrKey p]
  | XNixBinding !(XXNixBinding p)

deriving instance
  ( Data p,
    Data (XNixNormalBinding p),
    Data (NixAttrPath p),
    Data (NixExpr p),
    Data (XNixInheritBinding p),
    Data (XXNixBinding p),
    Data (NixAttrKey p)
  ) =>
  Data (NixBinding p)

deriving instance
  ( Show (XNixNormalBinding p),
    Show (NixAttrPath p),
    Show (NixExpr p),
    Show (XNixInheritBinding p),
    Show (XXNixBinding p),
    Show (NixAttrKey p)
  ) =>
  Show (NixBinding p)

type LNixBinding p = Located (NixBinding p)

type NixBindings p = [LNixBinding p]

type LNixBindings p = Located (NixBindings p)

type family XNixNormalBinding p

type family XNixInheritBinding p

type family XXNixBinding p

--------------------------------------------------------------------------------
data NixSetPatAsLocation
  = -- | @x@{...}@
    NixSetPatAsLeading
  | -- | @{...}@x@
    NixSetPatAsTrailing
  deriving (Eq, Show, Ord, Data)

--------------------------------------------------------------------------------

data NixSetPatAs p = NixSetPatAs
  { nspaAnn :: XNixSetPatAs p,
    nspaLocation :: NixSetPatAsLocation,
    -- | @x@{...}@
    nspaVar :: LNixId p
  }

deriving instance (Data p, Data (XNixSetPatAs p), Data (NixId p)) => Data (NixSetPatAs p)

deriving instance (Show (XNixSetPatAs p), Show (NixId p)) => Show (NixSetPatAs p)

type LNixSetPatAs p = Located (NixSetPatAs p)

--------------------------------------------------------------------------------

data NixSetPatBinding p = NixSetPatBinding
  { nspbAnn :: XNixSetPatBinding p,
    -- | @{a}@
    nspbVar :: LNixId p,
    -- | @{a ? b}@
    nspbDefault :: Maybe (LNixExpr p)
  }

deriving instance (Data p, Data (XNixSetPatBinding p), Data (NixId p), Data (NixExpr p)) => Data (NixSetPatBinding p)

deriving instance (Show (XNixSetPatBinding p), Show (NixId p), Show (NixExpr p)) => Show (NixSetPatBinding p)

type LNixSetPatBinding p = Located (NixSetPatBinding p)

--------------------------------------------------------------------------------

-- | Whether the pattern accepts unknown arguments
data NixSetPatEllipses
  = NixSetPatIsEllipses
  | NixSetPatNotEllipses
  deriving (Show, Eq, Ord, Data)

data NixFuncPat p
  = -- | @x: ...@
    NixVarPat (XNixVarPat p) (LNixId p)
  | -- | @x@{a, b ? c, d, ...}: ...@
    NixSetPat (XNixSetPat p) NixSetPatEllipses (Maybe (LNixSetPatAs p)) [LNixSetPatBinding p]
  | XNixFuncPat !(XXNixFuncPat p)

deriving instance
  ( Data p,
    Data (NixId p),
    Data (XNixVarPat p),
    Data (XNixSetPat p),
    Data (XXNixFuncPat p),
    Data (NixSetPatAs p),
    Data (NixSetPatBinding p)
  ) =>
  Data (NixFuncPat p)

deriving instance
  ( Show (NixId p),
    Show (XNixVarPat p),
    Show (XNixSetPat p),
    Show (XXNixFuncPat p),
    Show (NixSetPatAs p),
    Show (NixSetPatBinding p)
  ) =>
  Show (NixFuncPat p)

type LNixFuncPat p = Located (NixFuncPat p)

type family XNixVarPat p

type family XNixSetPat p

type family XNixSetPatAs p

type family XNixSetPatBinding p

type family XXNixFuncPat p

--------------------------------------------------------------------------------
data NixSetIsRecursive
  = -- | @rec {...}@
    NixSetRecursive
  | -- | @{...}@
    NixSetNonRecursive
  deriving (Show, Eq, Ord, Data)

--------------------------------------------------------------------------------

data NixExpr p
  = -- | @x@
    NixVar (XNixVar p) (LNixId p)
  | -- | See 'NixLit'
    NixLit (XNixLit p) (LNixLit p)
  | -- | Parenthesized expr - the pretty printer won't add parens
    NixPar (XNixPar p) (LNixExpr p)
  | -- | See 'NixString'
    NixString (XNixString p) (LNixString p)
  | -- | See 'NixPath'
    NixPath (XNixPath p) (LNixPath p)
  | -- | @<nixpkgs>@
    NixEnvPath (XNixEnvPath p) (Located Text)
  | -- | See 'LNixFuncPat'
    NixLam (XNixLam p) (LNixFuncPat p) (LNixExpr p)
  | -- | @f x@
    NixApp (XNixApp p) (LNixExpr p) (LNixExpr p)
  | -- | @a + b@, @a >= b@, @a // b@...
    NixBinApp (XNixBinApp p) BinaryOp (LNixExpr p) (LNixExpr p)
  | -- | @!a@
    NixNotApp (XNixNotApp p) (LNixExpr p)
  | -- | @-a@
    NixNegApp (XNixNegApp p) (LNixExpr p)
  | -- | @[ a b c ]@
    NixList (XNixList p) [LNixExpr p]
  | -- | See 'NixSetRecursive' and 'NixBinding'
    NixSet (XNixSet p) NixSetIsRecursive (LNixBindings p)
  | -- | @let a = 1; in b@, @let inherit (a) b; in c@
    -- Note: legacy let body syntax is not supported
    NixLet (XNixLet p) (LNixBindings p) (LNixExpr p)
  | -- | @a ? b@
    NixHasAttr (XNixHasAttr p) (LNixExpr p) (LNixAttrPath p)
  | -- | @a.b or c@
    NixSelect (XNixSelect p) (LNixExpr p) (LNixAttrPath p) (Maybe (LNixExpr p))
  | -- | @if a then b else c@
    NixIf (XNixIf p) (LNixExpr p) (LNixExpr p) (LNixExpr p)
  | -- | @with a; b@
    NixWith (XNixWith p) (LNixExpr p) (LNixExpr p)
  | -- | @assert a; b@
    NixAssert (XNixAssert p) (LNixExpr p) (LNixExpr p)
  | XNixExpr !(XXNixExpr p)

deriving instance
  ( Data p,
    Data (NixId p),
    Data (XNixVar p),
    Data (XNixLit p),
    Data (NixLit p),
    Data (XNixPar p),
    Data (XNixString p),
    Data (NixString p),
    Data (NixPath p),
    Data (XNixPath p),
    Data (XNixEnvPath p),
    Data (XNixLam p),
    Data (NixFuncPat p),
    Data (XNixApp p),
    Data (XNixBinApp p),
    Data (XNixNotApp p),
    Data (XNixNegApp p),
    Data (XNixList p),
    Data (XNixSet p),
    Data (NixBinding p),
    Data (XNixLet p),
    Data (XNixHasAttr p),
    Data (NixAttrPath p),
    Data (XNixSelect p),
    Data (XNixIf p),
    Data (XNixWith p),
    Data (XNixAssert p),
    Data (XXNixExpr p)
  ) =>
  Data (NixExpr p)

deriving instance
  ( Show (NixId p),
    Show (XNixVar p),
    Show (XNixLit p),
    Show (NixLit p),
    Show (XNixPar p),
    Show (XNixString p),
    Show (NixString p),
    Show (NixPath p),
    Show (XNixPath p),
    Show (XNixEnvPath p),
    Show (XNixLam p),
    Show (NixFuncPat p),
    Show (XNixApp p),
    Show (XNixBinApp p),
    Show (XNixNotApp p),
    Show (XNixNegApp p),
    Show (XNixList p),
    Show (XNixSet p),
    Show (NixBinding p),
    Show (XNixLet p),
    Show (XNixHasAttr p),
    Show (NixAttrPath p),
    Show (XNixSelect p),
    Show (XNixIf p),
    Show (XNixWith p),
    Show (XNixAssert p),
    Show (XXNixExpr p)
  ) =>
  Show (NixExpr p)

type family NixId p

type LNixId p = Located (NixId p)

type LNixExpr p = Located (NixExpr p)

type family XXNixExpr p

type family XNixApp p

type family XNixPath p

type family XNixEnvPath p

type family XNixString p

type family XNixBinApp p

type family XNixNotApp p

type family XNixNegApp p

type family XNixList p

type family XNixHasAttr p

type family XNixIf p

type family XNixWith p

type family XNixAssert p

type family XNixSelect p

type family XNixSet p

type family XNixLet p

type family XNixLam p

type family XNixVar p

type family XNixLit p

type family XNixPar p

--------------------------------------------------------------------------------
