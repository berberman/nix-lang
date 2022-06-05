{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Nix.Lang.Types where

import Data.Data (Data)
import Data.Text (Text)
import Data.Void (Void)

data SrcSpan = SrcSpan
  { srcSpanFilename :: String,
    srcSpanStartLine :: Int,
    srcSpanStartColumn :: Int,
    srcSpanEndLine :: Int,
    srcSpanEndColumn :: Int
  }
  deriving (Show, Eq, Ord, Data)

data Located a = L SrcSpan a
  deriving (Eq, Ord, Data, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------
data Ps

type instance NixIdP Ps = Text

type instance XNixUri Ps = ()

type instance XNixInteger Ps = ()

type instance XNixFloat Ps = ()

type instance XNixBoolean Ps = ()

type instance XNixNull Ps = ()

type instance XNixStringLiteral Ps = ()

type instance XNixStringInterpol Ps = ()

type instance XXNixStringPart Ps = Void

type instance XNixDoubleQuotesString Ps = ()

type instance XNixDoubleSingleQuotesString Ps = ()

type instance XXNixString Ps = Void

type instance XNixLiteralPath Ps = ()

type instance XNixInterpolPath Ps = ()

type instance XXNixPath Ps = Void

type instance XNixStaticAttrKey Ps = ()

type instance XNixDynamicStringAttrKey Ps = ()

type instance XNixDynamicInterpolAttrKey Ps = ()

type instance XXNixAttrKey Ps = Void

type instance XNixNormalBinding Ps = ()

type instance XNixInheritBinding Ps = ()

type instance XXNixBinding Ps = Void

type instance XNixVarPat Ps = ()

type instance XNixSetPat Ps = ()

type instance XXNixFuncPat Ps = Void

type instance XNixVar Ps = ()

type instance XNixLit Ps = ()

type instance XNixString Ps = ()

type instance XNixPath Ps = ()

type instance XNixEnvPath Ps = ()

type instance XNixLam Ps = ()

type instance XNixApp Ps = ()

type instance XNixBinApp Ps = ()

type instance XNixNotApp Ps = ()

type instance XNixNegApp Ps = ()

type instance XNixList Ps = ()

type instance XNixSet Ps = ()

type instance XNixLet Ps = ()

type instance XNixHasAttr Ps = ()

type instance XNixSelect Ps = ()

type instance XNixIf Ps = ()

type instance XNixWith Ps = ()

type instance XNixAssert Ps = ()

type instance XXNixExpr Ps = Void

type ParsedNixExpr = LNixExpr Ps

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
  | -- | @>@
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
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------
data NixLit p
  = -- | @https://nixos.org/@
    NixUri (XNixUri p) Text
  | -- | @233@
    NixInteger (XNixInteger p) Integer
  | -- | @233.3@
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

type LNixLit p = Located (NixLit p)

type family XNixUri p

type family XNixInteger p

type family XNixFloat p

type family XNixBoolean p

type family XNixNull p

type family XNixVar p

type family XNixLit p

type family XXNixLit p

--------------------------------------------------------------------------------

data NixStringPart p
  = -- | @x@
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
    NixDoubleSingleQuotesString (XNixDoubleSingleQuotesString p) Int [LNixStringPart p]
  | XNixString !(XXNixString p)

deriving instance
  ( Data p,
    Data (XNixDoubleQuotesString p),
    Data (XNixDoubleSingleQuotesString p),
    Data (XXNixString p),
    Data (NixStringPart p)
  ) =>
  Data (NixString p)

type LNixString p = Located (NixString p)

type family XNixDoubleQuotesString p

type family XNixDoubleSingleQuotesString p

type family XXNixString p

--------------------------------------------------------------------------------
data NixPath p
  = -- | @./a/b/c@, @/a/b/c@, @~/a/b/c@
    NixLiteralPath (XNixLiteralPath p) Text
  | -- | @./${e}/b/c@...
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

type LNixPath p = Located (NixPath p)

type family XNixLiteralPath p

type family XNixInterpolPath p

type family XXNixPath p

--------------------------------------------------------------------------------
data NixAttrKey p
  = -- | @{ x = 123; }.x@
    NixStaticAttrKey (XNixStaticAttrKey p) (LNixIdP p)
  | -- | @{ x = 123; }."x"@, @{ x = 123; }."${"x"}"@
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
    Data (NixIdP p),
    Data (NixExpr p)
  ) =>
  Data (NixAttrKey p)

type LNixAttrKey p = Located (NixAttrKey p)

type family XNixStaticAttrKey p

type family XNixDynamicStringAttrKey p

type family XNixDynamicInterpolAttrKey p

type family XXNixAttrKey p

--------------------------------------------------------------------------------

-- | @a.b.${c}."d"."${"e"}"@
newtype NixAttrPath p = NixAttrPath [LNixAttrKey p]

deriving instance (Data p, Data (NixAttrKey p)) => Data (NixAttrPath p)

type LNixAttrPath p = Located (NixAttrPath p)

--------------------------------------------------------------------------------

data NixBinding p
  = -- | @x = e;@
    NixNormalBinding (XNixNormalBinding p) (LNixAttrPath p) (LNixExpr p)
  | -- | @inherit a@, @inherit (a) b c@, @inherit (a) "b" ${"c"}@
    NixInheritBinding (XNixInheritBinding p) (Maybe (NixExpr p)) [LNixAttrKey p]
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

type LNixBinding p = Located (NixBinding p)

type family XNixNormalBinding p

type family XNixInheritBinding p

type family XXNixBinding p

--------------------------------------------------------------------------------

data NixSetPatAs p = NixSetPatAs
  { -- | true: @x@{...}@, false: @{...}@x@
    nspaIsLeading :: Bool,
    -- | @x@{...}@
    nspaVar :: LNixIdP p
  }

deriving instance (Data p, Data (NixIdP p)) => Data (NixSetPatAs p)

--------------------------------------------------------------------------------

data NixSetPatBinding p = NixSetPatAsBinding
  { -- | @{a, b}@
    nspbVar :: LNixIdP p,
    -- | @{a ? b}@
    nspbDefault :: Maybe (LNixExpr p)
  }

deriving instance (Data p, Data (NixIdP p), Data (NixExpr p)) => Data (NixSetPatBinding p)

type LNixSetPatBinding p = Located (NixSetPatBinding p)

--------------------------------------------------------------------------------

type NixSetPatEllipses = Bool

data NixFuncPat p
  = -- | @x: ...@
    NixVarPat (XNixVarPat p) (LNixIdP p)
  | -- | @x@{a, b ? c, d, ...}: ...@
    NixSetPat (XNixSetPat p) NixSetPatEllipses (Maybe (NixSetPatAs p)) [LNixSetPatBinding p]
  | XNixFuncPat !(XXNixFuncPat p)

deriving instance
  ( Data p,
    Data (NixIdP p),
    Data (XNixVarPat p),
    Data (XNixSetPat p),
    Data (XXNixFuncPat p),
    Data (NixSetPatAs p),
    Data (NixSetPatBinding p)
  ) =>
  Data (NixFuncPat p)

type LNixFuncPat p = Located (NixFuncPat p)

type family XNixVarPat p

type family XNixSetPat p

type family XXNixFuncPat p

--------------------------------------------------------------------------------

-- | true: @rec {...}@, false : @{...}@
type NixSetRecursive = Bool

data NixExpr p
  = -- | @x@
    NixVar (XNixVar p) (LNixIdP p)
  | -- | See 'NixLit'
    NixLit (XNixLit p) (LNixLit p)
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
  | -- | See 'NixSetRecursive' and 'LNixBinding'
    NixSet (XNixSet p) NixSetRecursive [LNixBinding p]
  | -- | @let a = 1; in b@, @let inherit (a) b; in c@
    NixLet (XNixLet p) [LNixBinding p] (LNixExpr p)
  | -- | @a ? b@
    NixHasAttr (XNixHasAttr p) (LNixExpr p) (LNixAttrPath p)
  | -- | See 'NixAttrPath'
    NixSelect (XNixSelect p) (Maybe (NixExpr p)) (LNixExpr p) (LNixAttrPath p)
  | -- | @if a then b else c@
    NixIf (XNixIf p) (LNixExpr p) (LNixExpr p) (LNixExpr p)
  | -- | @with a; b@
    NixWith (XNixWith p) (LNixExpr p) (LNixExpr p)
  | -- | @assert a; b@
    NixAssert (XNixAssert p) (LNixExpr p) (LNixExpr p)
  | XNixExpr !(XXNixExpr p)

deriving instance
  ( Data p,
    Data (NixIdP p),
    Data (XNixVar p),
    Data (XNixLit p),
    Data (NixLit p),
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

type family NixIdP p

type LNixIdP p = Located (NixIdP p)

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

--------------------------------------------------------------------------------
