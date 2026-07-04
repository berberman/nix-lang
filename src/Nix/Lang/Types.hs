{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core Trees-that-Grow Nix AST definitions.
--
-- This is the shared TTG definition that both syntax passes instantiate:
-- parser-produced trees with annotations, and fresh trees built for formatting.
module Nix.Lang.Types where

import Data.Data (Data)
import Data.Proxy (Proxy)
import Data.Text (Text)

--------------------------------------------------------------------------------
data NoExtF = NoExtF
  deriving (Eq, Show, Data)

data NoExtC
  deriving (Data)

instance Show NoExtC where
  show _ = undefined

type family XRec p a

class UnXRec p where
  unXRec :: Proxy p -> XRec p a -> a

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

type LNixLit p = XRec p (NixLit p)

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

type LNixStringPart p = XRec p (NixStringPart p)

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
    Data (LNixStringPart p)
  ) =>
  Data (NixString p)

deriving instance
  ( Show (XNixDoubleQuotesString p),
    Show (XNixDoubleSingleQuotesString p),
    Show (XXNixString p),
    Show (LNixStringPart p)
  ) =>
  Show (NixString p)

type LNixString p = XRec p (NixString p)

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

type LNixPath p = XRec p (NixPath p)

type family XNixLiteralPath p

type family XNixInterpolPath p

type family XXNixPath p

--------------------------------------------------------------------------------
data NixAttrKey p
  = -- | @{ x = 123; }.x@
    NixStaticAttrKey (XNixStaticAttrKey p) (LNixAttrName p)
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
    Data (LNixAttrName p),
    Data (LNixExpr p)
  ) =>
  Data (NixAttrKey p)

deriving instance
  ( Show (XNixStaticAttrKey p),
    Show (XNixDynamicStringAttrKey p),
    Show (XNixDynamicInterpolAttrKey p),
    Show (LNixStringPart p),
    Show (XXNixAttrKey p),
    Show (LNixAttrName p),
    Show (LNixExpr p)
  ) =>
  Show (NixAttrKey p)

type LNixAttrKey p = XRec p (NixAttrKey p)

type family XNixStaticAttrKey p

type family XNixDynamicStringAttrKey p

type family XNixDynamicInterpolAttrKey p

type family XXNixAttrKey p

--------------------------------------------------------------------------------

-- | @a.b.${c}."d"."${"e"}"@
data NixAttrPath p = NixAttrPath (XNixAttrPath p) [LNixAttrKey p]

deriving instance (Data p, Data (XNixAttrPath p), Data (LNixAttrKey p)) => Data (NixAttrPath p)

deriving instance (Show (XNixAttrPath p), Show (LNixAttrKey p)) => Show (NixAttrPath p)

type LNixAttrPath p = XRec p (NixAttrPath p)

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
    Data (LNixAttrPath p),
    Data (LNixExpr p),
    Data (XNixInheritBinding p),
    Data (XXNixBinding p),
    Data (LNixAttrKey p)
  ) =>
  Data (NixBinding p)

deriving instance
  ( Show (XNixNormalBinding p),
    Show (LNixAttrPath p),
    Show (LNixExpr p),
    Show (XNixInheritBinding p),
    Show (XXNixBinding p),
    Show (LNixAttrKey p)
  ) =>
  Show (NixBinding p)

type LNixBinding p = XRec p (NixBinding p)

type NixBindings p = [LNixBinding p]

type LNixBindings p = XRec p (NixBindings p)

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
    nspaVar :: LNixBinderName p
  }

deriving instance (Data p, Data (XNixSetPatAs p), Data (LNixBinderName p)) => Data (NixSetPatAs p)

deriving instance (Show (XNixSetPatAs p), Show (LNixBinderName p)) => Show (NixSetPatAs p)

type LNixSetPatAs p = XRec p (NixSetPatAs p)

--------------------------------------------------------------------------------

data NixSetPatBinding p = NixSetPatBinding
  { nspbAnn :: XNixSetPatBinding p,
    -- | @{a}@
    nspbVar :: LNixBinderName p,
    -- | @{a ? b}@
    nspbDefault :: Maybe (LNixExpr p)
  }

deriving instance (Data p, Data (XNixSetPatBinding p), Data (LNixBinderName p), Data (LNixExpr p)) => Data (NixSetPatBinding p)

deriving instance (Show (XNixSetPatBinding p), Show (LNixBinderName p), Show (LNixExpr p)) => Show (NixSetPatBinding p)

type LNixSetPatBinding p = XRec p (NixSetPatBinding p)

--------------------------------------------------------------------------------

-- | Whether the pattern accepts unknown arguments
data NixSetPatEllipses
  = NixSetPatIsEllipses
  | NixSetPatNotEllipses
  deriving (Show, Eq, Ord, Data)

data NixFuncPat p
  = -- | @x: ...@
    NixVarPat (XNixVarPat p) (LNixBinderName p)
  | -- | @x@{a, b ? c, d, ...}: ...@
    NixSetPat (XNixSetPat p) NixSetPatEllipses (Maybe (LNixSetPatAs p)) [LNixSetPatBinding p]
  | XNixFuncPat !(XXNixFuncPat p)

deriving instance
  ( Data p,
    Data (LNixBinderName p),
    Data (XNixVarPat p),
    Data (XNixSetPat p),
    Data (XXNixFuncPat p),
    Data (LNixSetPatAs p),
    Data (LNixSetPatBinding p)
  ) =>
  Data (NixFuncPat p)

deriving instance
  ( Show (LNixBinderName p),
    Show (XNixVarPat p),
    Show (XNixSetPat p),
    Show (XXNixFuncPat p),
    Show (LNixSetPatAs p),
    Show (LNixSetPatBinding p)
  ) =>
  Show (NixFuncPat p)

type LNixFuncPat p = XRec p (NixFuncPat p)

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
    NixVar (XNixVar p) (LNixVarName p)
  | -- | See 'NixLit'
    NixLit (XNixLit p) (LNixLit p)
  | -- | Parenthesized expr - the pretty printer won't add parens
    NixPar (XNixPar p) (LNixExpr p)
  | -- | See 'NixString'
    NixString (XNixString p) (LNixString p)
  | -- | See 'NixPath'
    NixPath (XNixPath p) (LNixPath p)
  | -- | @<nixpkgs>@
    NixEnvPath (XNixEnvPath p) (XRec p Text)
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
    Data (LNixVarName p),
    Data (XNixVar p),
    Data (XNixLit p),
    Data (LNixLit p),
    Data (XNixPar p),
    Data (LNixExpr p),
    Data (XNixString p),
    Data (LNixString p),
    Data (LNixPath p),
    Data (XNixPath p),
    Data (XNixEnvPath p),
    Data (XRec p Text),
    Data (XNixLam p),
    Data (LNixFuncPat p),
    Data (XNixApp p),
    Data (XNixBinApp p),
    Data (XNixNotApp p),
    Data (XNixNegApp p),
    Data (XNixList p),
    Data (XNixSet p),
    Data (LNixBindings p),
    Data (XNixLet p),
    Data (XNixHasAttr p),
    Data (LNixAttrPath p),
    Data (XNixSelect p),
    Data (XNixIf p),
    Data (XNixWith p),
    Data (XNixAssert p),
    Data (XXNixExpr p)
  ) =>
  Data (NixExpr p)

deriving instance
  ( Show (LNixVarName p),
    Show (XNixVar p),
    Show (XNixLit p),
    Show (LNixLit p),
    Show (XNixPar p),
    Show (LNixExpr p),
    Show (XNixString p),
    Show (LNixString p),
    Show (LNixPath p),
    Show (XNixPath p),
    Show (XNixEnvPath p),
    Show (XRec p Text),
    Show (XNixLam p),
    Show (LNixFuncPat p),
    Show (XNixApp p),
    Show (XNixBinApp p),
    Show (XNixNotApp p),
    Show (XNixNegApp p),
    Show (XNixList p),
    Show (XNixSet p),
    Show (LNixBindings p),
    Show (XNixLet p),
    Show (XNixHasAttr p),
    Show (LNixAttrPath p),
    Show (XNixSelect p),
    Show (XNixIf p),
    Show (XNixWith p),
    Show (XNixAssert p),
    Show (XXNixExpr p)
  ) =>
  Show (NixExpr p)

type family NixVarName p

type LNixVarName p = XRec p (NixVarName p)

type family NixBinderName p

type LNixBinderName p = XRec p (NixBinderName p)

type family NixAttrName p

type LNixAttrName p = XRec p (NixAttrName p)

type LNixExpr p = XRec p (NixExpr p)

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
