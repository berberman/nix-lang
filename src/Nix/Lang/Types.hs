{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Nix.Lang.Types where

import Data.Data (Data)
import Data.Text (Text)

data SrcSpan = SrcSpan
  { srcSpanFilename :: String,
    srcSpanStartLine :: Int,
    srcSpanStartColumn :: Int,
    srcSpanEndLine :: Int,
    srcSpanEndColumn :: Int
  }
  deriving (Eq, Data)

instance Show SrcSpan where
  show SrcSpan {..} =
    srcSpanFilename <> ":"
      <> if srcSpanStartLine == srcSpanEndLine
        then
          show srcSpanEndLine <> ":"
            <> if srcSpanStartColumn == srcSpanEndColumn
              then show srcSpanEndColumn
              else show srcSpanStartColumn <> "-" <> show srcSpanEndColumn
        else show (srcSpanStartLine, srcSpanStartColumn) <> "-" <> show (srcSpanEndLine, srcSpanEndColumn)

instance Ord SrcSpan where
  a `compare` b = case (srcSpanStartLine a, srcSpanStartColumn a)
    `compare` (srcSpanStartLine b, srcSpanStartColumn b) of
    EQ -> (srcSpanEndLine a, srcSpanEndColumn a) `compare` (srcSpanEndLine b, srcSpanEndColumn b)
    x -> x

data Located a = L SrcSpan a
  deriving (Eq, Show, Ord, Data, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

data Ann
  = -- | @assert@
    AnnAssert
  | -- | @if@
    AnnIf
  | -- | @else@
    AnnElse
  | -- | @then@
    AnnThen
  | -- | @let@
    AnnLet
  | -- | @in@
    AnnIn
  | -- | @inherit@
    AnnInherit
  | -- | @rec@
    AnnRec
  | -- | @with@
    AnnWith
  | -- | @{@
    AnnOpenC
  | -- | @}@
    AnnCloseC
  | -- | @[@
    AnnOpenS
  | -- | @]@
    AnnCloseS
  | -- | @(@
    AnnOpenP
  | -- | @)@
    AnnCloseP
  | -- | @=@
    AnnAssign
  | -- | @@@
    AnnAt
  | -- | @:@
    AnnColon
  | -- | @,@
    AnnComma
  | -- | @.@
    AnnDot
  | -- | @...@
    AnnEllipsis
  | -- | @?@
    AnnQuestion
  | -- | @;@
    AnnSemicolon
  | -- | @++@
    AnnConcat
  | -- | @//@
    AnnUpdate
  | -- | @!@
    AnnEx
  | -- | @+@
    AnnAdd
  | -- | @-@
    AnnSub
  | -- | @*@
    AnnMul
  | -- | @/@
    AnnDiv
  | -- | @&&@
    AnnAnd
  | -- | @||@
    AnnOr
  | -- | @->@
    AnnImpl
  | -- | @==@
    AnnEqual
  | -- | @!=@
    AnnNEqual
  | -- | @>@
    AnnGT
  | -- | @>=@
    AnnGE
  | -- | @<@
    AnnLT
  | -- | @<=@
    AnnLE
  | -- | Identifier
    AnnId
  | -- | Value
    AnnVal
  | -- | @${@
    AnnInterpolOpen
  | -- | @$}@
    AnnInterpolClose
  | -- | @<@
    AnnEnvPathOpen
  | -- | @>@
    AnnEnvPathClose
  | -- | @-@
    AnnNeg
  | -- | @''@
    AnnDoubleSingleQuotes
  | -- | @"@
    AnnDoubleQuote
  | -- | End of file
    AnnEof
  deriving (Show, Eq, Enum, Data)

data Comment
  = BlockComment Text
  | LineComment Text
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------
data Ps

newtype SourceText = SourceText Text
  deriving (Eq, Show, Ord, Data)

data NoExtF = NoExtF
  deriving (Eq, Show, Data)

data NoExtC
  deriving (Data)

instance Show NoExtC where
  show _ = undefined

type instance NixIdP Ps = Text

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

type instance XNixInterpolPath Ps = SourceText

type instance XXNixPath Ps = NoExtC

type instance XNixStaticAttrKey Ps = NoExtF

type instance XNixDynamicStringAttrKey Ps = SourceText

type instance XNixDynamicInterpolAttrKey Ps = SourceText

type instance XXNixAttrKey Ps = NoExtC

type instance XNixNormalBinding Ps = NoExtF

type instance XNixInheritBinding Ps = NoExtF

type instance XXNixBinding Ps = NoExtC

type instance XNixVarPat Ps = NoExtF

type instance XNixSetPat Ps = NoExtF

type instance XXNixFuncPat Ps = NoExtC

type instance XNixVar Ps = NoExtF

type instance XNixLit Ps = NoExtF

type instance XNixPar Ps = NoExtF

type instance XXNixLit Ps = NoExtC

type instance XNixString Ps = NoExtF

type instance XNixPath Ps = NoExtF

type instance XNixEnvPath Ps = NoExtF

type instance XNixLam Ps = NoExtF

type instance XNixApp Ps = NoExtF

type instance XNixBinApp Ps = NoExtF

type instance XNixNotApp Ps = NoExtF

type instance XNixNegApp Ps = NoExtF

type instance XNixList Ps = NoExtF

type instance XNixSet Ps = NoExtF

type instance XNixLet Ps = NoExtF

type instance XNixHasAttr Ps = NoExtF

type instance XNixSelect Ps = NoExtF

type instance XNixIf Ps = NoExtF

type instance XNixWith Ps = NoExtF

type instance XNixAssert Ps = NoExtF

type instance XXNixExpr Ps = NoExtC

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
    NixStaticAttrKey (XNixStaticAttrKey p) (LNixIdP p)
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
    Data (NixIdP p),
    Data (NixExpr p)
  ) =>
  Data (NixAttrKey p)

deriving instance
  ( Show (XNixStaticAttrKey p),
    Show (XNixDynamicStringAttrKey p),
    Show (XNixDynamicInterpolAttrKey p),
    Show (LNixStringPart p),
    Show (XXNixAttrKey p),
    Show (NixIdP p),
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
newtype NixAttrPath p = NixAttrPath [LNixAttrKey p]

deriving instance (Data p, Data (NixAttrKey p)) => Data (NixAttrPath p)

deriving instance (Show (NixAttrKey p)) => Show (NixAttrPath p)

type LNixAttrPath p = Located (NixAttrPath p)

--------------------------------------------------------------------------------

data NixBinding p
  = -- | @x = e;@
    NixNormalBinding (XNixNormalBinding p) (LNixAttrPath p) (LNixExpr p)
  | -- | @inherit a@, @inherit (a) b c@, @inherit (a) "b" ${"c"}@
    -- Note: Dynamic keys like @${"a" + "b"}@ are not allowed by nix runtime, but only parsed.
    -- However, @${"a"}@ can be parsed and evaluated. For us, we accept them all.
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
  { nspaLocation :: NixSetPatAsLocation,
    -- | @x@{...}@
    nspaVar :: LNixIdP p
  }

deriving instance (Data p, Data (NixIdP p)) => Data (NixSetPatAs p)

deriving instance (Show (NixIdP p)) => Show (NixSetPatAs p)

--------------------------------------------------------------------------------

data NixSetPatBinding p = NixSetPatAsBinding
  { -- | @{a, b}@
    nspbVar :: LNixIdP p,
    -- | @{a ? b}@
    nspbDefault :: Maybe (LNixExpr p)
  }

deriving instance (Data p, Data (NixIdP p), Data (NixExpr p)) => Data (NixSetPatBinding p)

deriving instance (Show (NixIdP p), Show (NixExpr p)) => Show (NixSetPatBinding p)

type LNixSetPatBinding p = Located (NixSetPatBinding p)

--------------------------------------------------------------------------------

-- | Whether the pattern accepts unknown arguments
data NixSetPatEllipses
  = NixSetPatIsEllipses
  | NixSetPatNotEllipses
  deriving (Show, Eq, Ord, Data)

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

deriving instance
  ( Show (NixIdP p),
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
    NixVar (XNixVar p) (LNixIdP p)
  | -- | See 'NixLit'
    NixLit (XNixLit p) (LNixLit p)
  | -- | Parenthesized expr
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
    NixSet (XNixSet p) NixSetIsRecursive [LNixBinding p]
  | -- | @let a = 1; in b@, @let inherit (a) b; in c@
    NixLet (XNixLet p) [LNixBinding p] (LNixExpr p)
  | -- | @a ? b@
    NixHasAttr (XNixHasAttr p) (LNixExpr p) (LNixAttrPath p)
  | -- | See 'NixAttrPath'
    NixSelect (XNixSelect p) (Maybe (LNixExpr p)) (LNixExpr p) (LNixAttrPath p)
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
  ( Show (NixIdP p),
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

type family XNixVar p

type family XNixLit p

type family XNixPar p

--------------------------------------------------------------------------------
