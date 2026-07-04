{-# LANGUAGE DeriveDataTypeable #-}

-- | Annotation and exact-print support types for the parsed Nix AST.
--
-- These types record token spans, token deltas, and comment ownership for the
-- parsed pass. Exact printing and edit repair both work by preserving or
-- rebuilding this information.
module Nix.Lang.Annotation where

import Data.Data (Data)
import Data.Text (Text)
import Nix.Lang.Span

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
  | -- | Identifier-like annotation marker.
    AnnId
  | -- | Value-like annotation marker.
    AnnVal
  | -- | @${@
    AnnInterpolOpen
  | -- | @}@ closing an interpolation.
    AnnInterpolClose
  | -- | @<@ opening an environment path.
    AnnEnvPathOpen
  | -- | @>@ closing an environment path.
    AnnEnvPathClose
  | -- | Unary negation token @-@.
    AnnNeg
  | -- | @''@
    AnnDoubleSingleQuotes
  | -- | @"@
    AnnDoubleQuote
  | -- | End-of-file marker.
    AnnEof
  deriving (Show, Eq, Enum, Data)

--------------------------------------------------------------------------------

-- | Source comment as parsed from the input stream.
data Comment
  = BlockComment Text
  | LineComment Text
  deriving (Show, Eq, Data)

-- | Comments owned by a node.
--
-- 'priorComments' are comments that conceptually appear before the node and
-- should be emitted before the node in an exact printer. 'followingComments'
-- are reserved for later transformations/moves where comments need to travel
-- with a node after it.
data NodeComments = NodeComments
  { priorComments :: [Located Comment],
    followingComments :: [Located Comment]
  }
  deriving (Show, Eq, Data)

-- | Empty comment ownership for nodes that do not currently carry comments.
emptyComments :: NodeComments
emptyComments = NodeComments [] []

--------------------------------------------------------------------------------

-- | Relative position used by exact-print-aware annotations.
--
-- 'DeltaPos' is the compact form used once a token or node no longer needs to
-- remember its original absolute span. It describes how far the next emitted
-- syntax element is from the current printing anchor.
data DeltaPos = DeltaPos
  { deltaLine :: Int,
    deltaColumn :: Int
  }
  deriving (Show, Eq, Data)

-- | Position model shared by node-local annotations.
--
-- Parsed trees initially use 'AnnSpan'. Later exact-print-oriented passes may
-- rewrite positions to 'AnnDelta' while preserving the same typed annotation
-- payloads and AST shape.
data AnnPos
  = AnnSpan SrcSpan
  | AnnDelta DeltaPos
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Exact-print metadata for a single concrete syntax token.
data AnnToken = AnnToken
  { annToken :: Ann,
    annTokenPos :: AnnPos
  }
  deriving (Show, Eq, Data)

-- | Construct a parser-produced token annotation backed by an absolute span.
parsedAnnToken :: Ann -> SrcSpan -> AnnToken
parsedAnnToken tok src = AnnToken tok (AnnSpan src)

-- | Construct a token annotation backed by a relative exact-print delta.
deltaAnnToken :: Ann -> DeltaPos -> AnnToken
deltaAnnToken tok delta = AnnToken tok (AnnDelta delta)

-- | Recover the absolute span when a token still carries parser-originated
-- location data.
annTokenSrcSpan :: AnnToken -> Maybe SrcSpan
annTokenSrcSpan AnnToken {annTokenPos = AnnSpan src} = Just src
annTokenSrcSpan _ = Nothing

-- | Recover the relative delta when a token has been normalized for exact
-- printing.
annTokenDelta :: AnnToken -> Maybe DeltaPos
annTokenDelta AnnToken {annTokenPos = AnnDelta delta} = Just delta
annTokenDelta _ = Nothing

setAnnTokenDelta :: DeltaPos -> AnnToken -> AnnToken
setAnnTokenDelta delta tok = tok {annTokenPos = AnnDelta delta}

--------------------------------------------------------------------------------

-- | Shared payload reused across many node-local annotation records.
data AnnCommon = AnnCommon
  { acComments :: NodeComments,
    acPos :: AnnPos
  }
  deriving (Show, Eq, Data)

class HasAnnCommon a where
  getAnnCommon :: a -> AnnCommon
  setAnnCommon :: AnnCommon -> a -> a

-- | Get the owned comments for a typed annotation payload.
annComments :: (HasAnnCommon a) => a -> NodeComments
annComments = acComments . getAnnCommon

-- | Get the common exact-print position for a typed annotation payload.
annPos :: (HasAnnCommon a) => a -> AnnPos
annPos = acPos . getAnnCommon

-- | Get the absolute source span when the common position is parser-produced.
annSrcSpan :: (HasAnnCommon a) => a -> Maybe SrcSpan
annSrcSpan ann = case annPos ann of
  AnnSpan l -> Just l
  AnnDelta _ -> Nothing

setAnnSpan :: (HasAnnCommon a) => SrcSpan -> a -> a
setAnnSpan span' ann = setAnnCommon ((getAnnCommon ann) {acPos = AnnSpan span'}) ann

--------------------------------------------------------------------------------

-- | Annotation payload for sets and recursive sets.
data AnnSet = AnnSet
  { asCommon :: AnnCommon,
    asRec :: Maybe AnnToken,
    asOpenC :: AnnToken,
    asCloseC :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for @let ... in ...@.
data AnnLetNode = AnnLetNode
  { alCommon :: AnnCommon,
    alLet :: AnnToken,
    alIn :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for @if ... then ... else ...@.
data AnnIfNode = AnnIfNode
  { aifCommon :: AnnCommon,
    aifIf :: AnnToken,
    aifThen :: AnnToken,
    aifElse :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for @with ...; ...@.
data AnnWithNode = AnnWithNode
  { awCommon :: AnnCommon,
    awWith :: AnnToken,
    awSemicolon :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for @assert ...; ...@.
data AnnAssertNode = AnnAssertNode
  { aaCommon :: AnnCommon,
    aaAssert :: AnnToken,
    aaSemicolon :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for selection expressions, including optional @or@.
data AnnSelect = AnnSelect
  { aslCommon :: AnnCommon,
    aslOr :: Maybe AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for @a ? b@.
data AnnHasAttr = AnnHasAttr
  { ahaCommon :: AnnCommon,
    ahaQuestion :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for normal bindings like @x = e;@.
data AnnNormalBinding = AnnNormalBinding
  { anbCommon :: AnnCommon,
    anbEqual :: AnnToken,
    anbSemicolon :: AnnToken
  }
  deriving (Show, Eq, Data)

-- | Annotation payload for @inherit ...;@ bindings.
data AnnInheritBinding = AnnInheritBinding
  { aibCommon :: AnnCommon,
    aibInherit :: AnnToken,
    aibSemicolon :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for variable patterns like @x:@.
data AnnVarPat = AnnVarPat
  { avpCommon :: AnnCommon,
    avpId :: SrcSpan
  }
  deriving (Show, Eq, Data)

-- | Annotation payload for set patterns.
data AnnSetPatNode = AnnSetPatNode
  { aspCommon :: AnnCommon,
    aspOpenC :: AnnToken,
    aspCloseC :: AnnToken,
    aspEllipsis :: Maybe AnnToken,
    aspCommas :: [AnnToken]
  }
  deriving (Show, Eq, Data)

data AnnSetPatAs = AnnSetPatAs
  { aspaCommon :: AnnCommon,
    aspaAt :: AnnToken
  }
  deriving (Show, Eq, Data)

data AnnSetPatBinding = AnnSetPatBinding
  { aspbCommon :: AnnCommon,
    aspbQuestion :: Maybe AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for dotted attribute paths.
data AnnAttrPath = AnnAttrPath
  { aapCommon :: AnnCommon,
    aapDots :: [AnnToken]
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for parenthesized expressions.
data AnnParNode = AnnParNode
  { apnCommon :: AnnCommon,
    apnOpenP :: AnnToken,
    apnCloseP :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for list expressions.
data AnnListNode = AnnListNode
  { alnCommon :: AnnCommon,
    alnOpenS :: AnnToken,
    alnCloseS :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for environment paths like @<nixpkgs>@.
data AnnEnvPathNode = AnnEnvPathNode
  { aenvCommon :: AnnCommon,
    aenvOpen :: AnnToken,
    aenvClose :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for string wrapper nodes.
newtype AnnStringNode = AnnStringNode
  { astrCommon :: AnnCommon
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for path wrapper nodes.
newtype AnnPathNode = AnnPathNode
  { apathCommon :: AnnCommon
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for lambda wrapper nodes.
data AnnLamNode = AnnLamNode
  { alamCommon :: AnnCommon,
    alamColon :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for application wrapper nodes.
data AnnAppNode = AnnAppNode
  { aappCommon :: AnnCommon
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for binary operator applications.
data AnnBinAppNode = AnnBinAppNode
  { abinCommon :: AnnCommon,
    abinOperator :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

-- | Annotation payload for prefix operator applications.
data AnnPrefixNode = AnnPrefixNode
  { apfxCommon :: AnnCommon,
    apfxToken :: AnnToken
  }
  deriving (Show, Eq, Data)

--------------------------------------------------------------------------------

instance HasAnnCommon AnnCommon where
  getAnnCommon = id
  setAnnCommon = const

instance HasAnnCommon AnnSet where
  getAnnCommon = asCommon
  setAnnCommon common ann = ann {asCommon = common}

instance HasAnnCommon AnnLetNode where
  getAnnCommon = alCommon
  setAnnCommon common ann = ann {alCommon = common}

instance HasAnnCommon AnnIfNode where
  getAnnCommon = aifCommon
  setAnnCommon common ann = ann {aifCommon = common}

instance HasAnnCommon AnnWithNode where
  getAnnCommon = awCommon
  setAnnCommon common ann = ann {awCommon = common}

instance HasAnnCommon AnnAssertNode where
  getAnnCommon = aaCommon
  setAnnCommon common ann = ann {aaCommon = common}

instance HasAnnCommon AnnSelect where
  getAnnCommon = aslCommon
  setAnnCommon common ann = ann {aslCommon = common}

instance HasAnnCommon AnnHasAttr where
  getAnnCommon = ahaCommon
  setAnnCommon common ann = ann {ahaCommon = common}

instance HasAnnCommon AnnNormalBinding where
  getAnnCommon = anbCommon
  setAnnCommon common ann = ann {anbCommon = common}

instance HasAnnCommon AnnInheritBinding where
  getAnnCommon = aibCommon
  setAnnCommon common ann = ann {aibCommon = common}

instance HasAnnCommon AnnVarPat where
  getAnnCommon = avpCommon
  setAnnCommon common ann = ann {avpCommon = common}

instance HasAnnCommon AnnSetPatNode where
  getAnnCommon = aspCommon
  setAnnCommon common ann = ann {aspCommon = common}

instance HasAnnCommon AnnSetPatAs where
  getAnnCommon = aspaCommon
  setAnnCommon common ann = ann {aspaCommon = common}

instance HasAnnCommon AnnSetPatBinding where
  getAnnCommon = aspbCommon
  setAnnCommon common ann = ann {aspbCommon = common}

instance HasAnnCommon AnnAttrPath where
  getAnnCommon = aapCommon
  setAnnCommon common ann = ann {aapCommon = common}

instance HasAnnCommon AnnParNode where
  getAnnCommon = apnCommon
  setAnnCommon common ann = ann {apnCommon = common}

instance HasAnnCommon AnnListNode where
  getAnnCommon = alnCommon
  setAnnCommon common ann = ann {alnCommon = common}

instance HasAnnCommon AnnEnvPathNode where
  getAnnCommon = aenvCommon
  setAnnCommon common ann = ann {aenvCommon = common}

instance HasAnnCommon AnnStringNode where
  getAnnCommon = astrCommon
  setAnnCommon common ann = ann {astrCommon = common}

instance HasAnnCommon AnnPathNode where
  getAnnCommon = apathCommon
  setAnnCommon common ann = ann {apathCommon = common}

instance HasAnnCommon AnnLamNode where
  getAnnCommon = alamCommon
  setAnnCommon common ann = ann {alamCommon = common}

instance HasAnnCommon AnnAppNode where
  getAnnCommon = aappCommon
  setAnnCommon common ann = ann {aappCommon = common}

instance HasAnnCommon AnnBinAppNode where
  getAnnCommon = abinCommon
  setAnnCommon common ann = ann {abinCommon = common}

instance HasAnnCommon AnnPrefixNode where
  getAnnCommon = apfxCommon
  setAnnCommon common ann = ann {apfxCommon = common}
