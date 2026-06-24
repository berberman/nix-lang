{-# LANGUAGE DeriveDataTypeable #-}

module Nix.Lang.Types.Rename where

import Data.Function (on)
import Data.Generics (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Nix.Lang.Span
import Nix.Lang.Types

newtype Unique = Unique Int
  deriving stock (Eq, Ord, Show, Data)

newtype OccName = OccName Text
  deriving stock (Eq, Ord, Show, Data)

data RnName = RnName
  { nameUnique :: Unique,
    nameOcc :: OccName
  }
  deriving stock (Show, Data)

instance Eq RnName where
  (==) = (==) `on` nameUnique

instance Ord RnName where
  compare = compare `on` nameUnique

newtype WithScopeId = WithScopeId Int
  deriving stock (Eq, Ord, Show, Data)

data UnresolvedReason
  = Unbound
  | ViaWith !(NonEmpty WithScopeId)
  deriving stock (Eq, Ord, Show, Data)

data RnVarName
  = LclName RnName
  | GlbName OccName
  | UnresolvedName UnresolvedReason OccName
  deriving stock (Eq, Ord, Show, Data)

newtype RnBinderName = ResBinderName
  { binderName :: RnName
  }
  deriving stock (Eq, Ord, Show, Data)

-- | Attr name can be binding, e.g. the @foo@ in @foo.bar = 1;@
-- or non-binding, e.g. the @bar@ in @foo.bar = 1;@.
-- We keep occ names for @foo@ and @bar@ here and store rn name of the binding attr root in 'RnNormalBinding'.
newtype RnAttrName = ResAttrName
  { attrOccName :: OccName
  }
  deriving stock (Eq, Ord, Show, Data)

-- | For bindings like @let x = 1; in ...@ or @let x.y = 1; in ...@
-- we store the only binding name, namely @x@' here.
newtype RnNormalBinding = RnNormalBinding
  { rootBinder :: Maybe RnName
  }
  deriving stock (Eq, Ord, Show, Data)

-- | Each with scope gets a unique id
newtype RnWith = RnWith
  { withScopeId :: WithScopeId
  }
  deriving stock (Eq, Ord, Show, Data)

newtype RnInheritBinding = RnInheritBinding
  { ribItems :: [RnInheritItem]
  }

-- | Whether an attr in inherit is binding.
-- For example, in @ rec {inherit a; b = a; }@ @a@ introduces a binder, but @{ inherit a; }@ doesn't.
-- The idea is similar to 'RnNormalBinding', but this type is for inherit bindings. 
newtype RnInheritItem = RnInheritItem
  { riiBinder :: Maybe RnName
  }

data Rn deriving (Data)

type instance XRec Rn a = Located a

instance UnXRec Rn where
  unXRec _ (L _ x) = x

type instance NixVarName Rn = RnVarName

type instance NixBinderName Rn = RnBinderName

type instance NixAttrName Rn = RnAttrName

type instance XNixUri Rn = NoExtF

type instance XNixInteger Rn = NoExtF

type instance XNixFloat Rn = NoExtF

type instance XNixBoolean Rn = NoExtF

type instance XNixNull Rn = NoExtF

type instance XNixStringLiteral Rn = NoExtF

type instance XNixStringInterpol Rn = NoExtF

type instance XXNixStringPart Rn = NoExtC

type instance XNixDoubleQuotesString Rn = NoExtF

type instance XNixDoubleSingleQuotesString Rn = NoExtF

type instance XXNixString Rn = NoExtC

type instance XNixLiteralPath Rn = NoExtF

type instance XNixInterpolPath Rn = NoExtF

type instance XXNixPath Rn = NoExtC

type instance XNixStaticAttrKey Rn = NoExtF

type instance XNixDynamicStringAttrKey Rn = NoExtF

type instance XNixDynamicInterpolAttrKey Rn = NoExtF

type instance XXNixAttrKey Rn = NoExtC

type instance XNixAttrPath Rn = NoExtF

type instance XNixNormalBinding Rn = RnNormalBinding

type instance XNixInheritBinding Rn = NoExtF

type instance XXNixBinding Rn = NoExtC

type instance XNixVarPat Rn = NoExtF

type instance XNixSetPat Rn = NoExtF

type instance XNixSetPatAs Rn = NoExtF

type instance XNixSetPatBinding Rn = NoExtF

type instance XXNixFuncPat Rn = NoExtC

type instance XNixVar Rn = NoExtF

type instance XNixLit Rn = NoExtF

type instance XNixPar Rn = NoExtF

type instance XXNixLit Rn = NoExtC

type instance XNixString Rn = NoExtF

type instance XNixPath Rn = NoExtF

type instance XNixEnvPath Rn = NoExtF

type instance XNixLam Rn = NoExtF

type instance XNixApp Rn = NoExtF

type instance XNixBinApp Rn = NoExtF

type instance XNixNotApp Rn = NoExtF

type instance XNixNegApp Rn = NoExtF

type instance XNixList Rn = NoExtF

type instance XNixSet Rn = NoExtF

type instance XNixLet Rn = NoExtF

type instance XNixHasAttr Rn = NoExtF

type instance XNixSelect Rn = NoExtF

type instance XNixIf Rn = NoExtF

type instance XNixWith Rn = RnWith

type instance XNixAssert Rn = NoExtF

type instance XXNixExpr Rn = NoExtC
