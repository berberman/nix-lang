{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Quasiquoting Nix expressions as fresh 'Nix.Lang.Types.Syn.Expr' values.
--
-- This quasiquoter is for /construction/, not exact-source recovery.
-- It parses Nix syntax, lowers it to the annotation-free 'Syn.Expr' AST, and
-- supports Haskell antiquotation with @%%(...)@ in expression positions.
--
-- Basic usage:
--
-- @
-- {-# LANGUAGE QuasiQuotes #-}
--
-- import Nix.Lang.QQ (nixQQ)
-- import Nix.Lang.Types.Syn (Expr)
--
-- expr :: Expr
-- expr = [nixQQ| let x = 1; in x |]
-- @
--
-- Antiquotation can splice existing 'Syn.Expr' values:
--
-- @
-- import Nix.Lang.Types.Syn (Expr, mkInt)
--
-- expr :: Expr
-- expr =
--   let value = mkInt 233
--    in [nixQQ| { x = %%(value); } |]
-- @
--
-- Antiquotation also accepts a small set of literal-like Haskell values via
-- 'ToNixExpr':
--
-- @
-- import Data.Text (Text)
-- import Nix.Lang.Types.Syn (Expr)
--
-- stringExpr :: Expr
-- stringExpr =
--   let value = ("hello" :: Text)
--    in [nixQQ| %%(value) |]
--
-- intExpr :: Expr
-- intExpr = [nixQQ| %%(233 :: Integer) |]
-- @
--
-- Note that textual antiquotation becomes a /Nix string literal/, not a Nix
-- identifier. If you want a variable, keep it explicit with smart
-- constructors such as 'Nix.Lang.Types.Syn.mkVar'.
--
-- For parsed/editable syntax trees instead of fresh 'Syn.Expr' construction,
-- use 'Nix.Lang.QQ.Parsed.nixParsedQQ' from the core @nix-lang@ package.
module Nix.Lang.QQ (ToNixExpr (..), nixQQ) where

import Control.Monad (void, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import Data.Void (Void)
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH hiding (Lit, match)
import Language.Haskell.TH.Quote
import Nix.Lang.Parser (located, nixFile, runNixParser)
import Nix.Lang.Span (Located)
import Nix.Lang.Types
import qualified Nix.Lang.Types.Ps as Ps
import qualified Nix.Lang.Types.Syn as Syn
import Nix.Lang.Utils (stripCommonPrefix, unLoc)
import Text.Megaparsec

class ToNixExpr a where
  toNixExpr :: a -> Syn.Expr

instance ToNixExpr Syn.Expr where
  toNixExpr = id

instance ToNixExpr Integer where
  toNixExpr = Syn.mkInt

instance ToNixExpr Int where
  toNixExpr = Syn.mkInt . fromIntegral

instance ToNixExpr Float where
  toNixExpr = Syn.mkFloat

instance ToNixExpr Bool where
  toNixExpr = Syn.mkBool

instance ToNixExpr T.Text where
  toNixExpr = Syn.mkText

instance ToNixExpr String where
  toNixExpr = Syn.mkText . T.pack

nixQQ :: QuasiQuoter
nixQQ =
  QuasiQuoter
    { quoteExp = nixQuot,
      quotePat = const $ error "nixQQ: unsupported quotePat",
      quoteType = const $ error "nixQQ: unsupported quoteType",
      quoteDec = const $ error "nixQQ: unsupported quoteDec"
    }

nixQuot :: String -> ExpQ
nixQuot src = do
  case rewriteAntiquotes (T.pack (stripCommonPrefix src)) of
    Left err -> fail err
    Right (rewritten, antiquotes) ->
      case runNixParser (located nixFile) "<qq>" rewritten of
        (Left err, _) -> fail (errorBundlePretty err)
        (Right expr, _) -> lowerExpr antiquotes expr

rewriteAntiquotes :: T.Text -> Either String (T.Text, IntMap Exp)
rewriteAntiquotes input = do
  chunks <- parseAntiquoteChunks input
  go 0 [] IntMap.empty chunks
  where
    go _ acc antiMap [] = Right (T.concat (reverse acc), antiMap)
    go next acc antiMap (PlainChunk text : rest) =
      go next (text : acc) antiMap rest
    go next acc antiMap (AntiquoteChunk payload : rest) = do
      parsedExp <- parseAntiquoteExp payload
      let (placeholderIdx, name) = nextPlaceholder input next
      go (placeholderIdx + 1) (name : acc) (IntMap.insert placeholderIdx parsedExp antiMap) rest

parseAntiquoteExp :: T.Text -> Either String Exp
parseAntiquoteExp payload =
  case parseExp (T.unpack payload) of
    Left err -> Left ("failed to parse nixQQ antiquotation: " <> err)
    Right expr -> Right expr

data QQChunk
  = PlainChunk T.Text
  | AntiquoteChunk T.Text

type QQParser = Parsec Void T.Text

parseAntiquoteChunks :: T.Text -> Either String [QQChunk]
parseAntiquoteChunks input =
  case parse (many qqChunk <* eof) "<nixQQ antiquotation>" input of
    Left err -> Left (errorBundlePretty err)
    Right chunks -> Right chunks

qqChunk :: QQParser QQChunk
qqChunk = choice [try antiquoteChunk, plainChunk]

plainChunk :: QQParser QQChunk
plainChunk = PlainChunk . T.concat <$> some plainPiece
  where
    plainPiece =
      choice
        [ takeWhile1P (Just "plain nixQQ chunk") (/= '%'),
          T.singleton <$> (notFollowedBy antiquoteStart *> single '%')
        ]

antiquoteChunk :: QQParser QQChunk
antiquoteChunk = AntiquoteChunk <$> (antiquoteStart *> payloadUntilMatchingClose)

antiquoteStart :: QQParser T.Text
antiquoteStart = chunk "%%("

-- TODO: do syntax aware lexing properly...
payloadUntilMatchingClose :: QQParser T.Text
payloadUntilMatchingClose = T.concat <$> go (1 :: Int)
  where
    go depth = do
      atInputEnd <- atEnd
      when atInputEnd $ fail "unterminated nixQQ antiquotation"
      c <- anySingle
      case c of
        '(' -> (T.singleton c :) <$> go (depth + 1)
        ')' ->
          if depth == 1
            then pure []
            else (T.singleton c :) <$> go (depth - 1)
        '"' -> do
          rest <- quotedTail '"'
          (T.cons c rest :) <$> go depth
        '\'' -> do
          rest <- quotedTail '\''
          (T.cons c rest :) <$> go depth
        _ -> (T.singleton c :) <$> go depth

quotedTail :: Char -> QQParser T.Text
quotedTail delim = fmap fst . match $ manyTill quotedPiece quotedEnd
  where
    quotedPiece = choice [escapedPiece, ordinaryPiece]
    escapedPiece = do
      void (chunk "\\")
      escapedEnd <- atEnd
      when escapedEnd $ fail "unterminated escape in nixQQ antiquotation"
      escaped <- anySingle
      pure (T.pack ['\\', escaped])
    ordinaryPiece = do
      piece <- takeWhile1P (Just "quoted antiquotation payload") (\c -> c /= '\\' && c /= delim)
      pure piece
    quotedEnd = do
      ended <- atEnd
      when ended $ fail "unterminated string in nixQQ antiquotation"
      void (single delim)

placeholderPrefix :: T.Text
placeholderPrefix = "__nix_lang_qq_antiquote_"

mkPlaceholderName :: Int -> T.Text
mkPlaceholderName n = placeholderPrefix <> T.pack (show n)

nextPlaceholder :: T.Text -> Int -> (Int, T.Text)
nextPlaceholder input = go
  where
    go idx =
      let name = mkPlaceholderName idx
       in if T.isInfixOf name input then go (idx + 1) else (idx, name)

lookupPlaceholder :: IntMap Exp -> T.Text -> Maybe Exp
lookupPlaceholder antiMap name = placeholderIndex name >>= (`IntMap.lookup` antiMap)

placeholderIndex :: T.Text -> Maybe Int
placeholderIndex name = do
  suffix <- T.stripPrefix placeholderPrefix name
  case reads (T.unpack suffix) of
    [(n, "")] -> Just n
    _ -> Nothing

lowerExpr :: IntMap Exp -> Located Ps.Expr -> ExpQ
lowerExpr antiMap = lowerExpr' antiMap . unLoc

lowerExpr' :: IntMap Exp -> Ps.Expr -> ExpQ
lowerExpr' antiMap = \case
  NixVar _ name ->
    case lookupPlaceholder antiMap (unLoc name) of
      Just expr -> [|toNixExpr $(pure expr)|]
      _ -> [|Syn.mkVar $(liftText (unLoc name))|]
  NixLit _ lit -> [|Syn.mkLit $(lowerLit (unLoc lit))|]
  NixPar _ expr -> [|Syn.mkPar $(lowerExpr antiMap expr)|]
  NixString _ str -> [|Syn.mkString $(lowerString antiMap (unLoc str))|]
  NixPath _ path -> [|Syn.mkPath $(lowerPath antiMap (unLoc path))|]
  NixEnvPath _ path -> [|Syn.mkEnvPath $(liftText (unLoc path))|]
  NixLam _ pat body -> [|Syn.mkLam $(lowerFuncPat antiMap (unLoc pat)) $(lowerExpr antiMap body)|]
  NixApp _ f x -> [|Syn.mkApp $(lowerExpr antiMap f) $(lowerExpr antiMap x)|]
  NixBinApp _ op lhs rhs -> [|Syn.mkBinApp $(lowerBinaryOp op) $(lowerExpr antiMap lhs) $(lowerExpr antiMap rhs)|]
  NixNotApp _ expr -> [|Syn.mkNot $(lowerExpr antiMap expr)|]
  NixNegApp _ expr -> [|Syn.mkNeg $(lowerExpr antiMap expr)|]
  NixList _ xs -> [|Syn.mkList $(lowerLocatedList antiMap xs)|]
  NixSet _ kind bindings ->
    case kind of
      NixSetRecursive -> [|Syn.mkRecSet $(lowerBindingList antiMap (unLoc bindings))|]
      NixSetNonRecursive -> [|Syn.mkSet $(lowerBindingList antiMap (unLoc bindings))|]
  NixLet _ bindings expr -> [|Syn.mkLet $(lowerBindingList antiMap (unLoc bindings)) $(lowerExpr antiMap expr)|]
  NixHasAttr _ expr path -> [|Syn.mkHasAttr $(lowerExpr antiMap expr) $(lowerAttrPath antiMap (unLoc path))|]
  NixSelect _ expr path Nothing -> [|Syn.mkSelect $(lowerExpr antiMap expr) $(lowerAttrPath antiMap (unLoc path))|]
  NixSelect _ expr path (Just fallback) -> [|Syn.mkSelectOr $(lowerExpr antiMap expr) $(lowerAttrPath antiMap (unLoc path)) $(lowerExpr antiMap fallback)|]
  NixIf _ cond t e -> [|Syn.mkIf $(lowerExpr antiMap cond) $(lowerExpr antiMap t) $(lowerExpr antiMap e)|]
  NixWith _ scope expr -> [|Syn.mkWith $(lowerExpr antiMap scope) $(lowerExpr antiMap expr)|]
  NixAssert _ assertion expr -> [|Syn.mkAssert $(lowerExpr antiMap assertion) $(lowerExpr antiMap expr)|]

lowerLit :: Ps.Lit -> ExpQ
lowerLit = \case
  NixUri _ uri -> [|Syn.mkUriLit $(liftText uri)|]
  NixInteger _ int -> [|Syn.mkIntegerLit int|]
  NixFloat _ float -> [|Syn.mkFloatLit float|]
  NixBoolean _ bool -> [|Syn.mkBooleanLit bool|]
  NixNull _ -> [|Syn.mkNullLit|]

lowerString :: IntMap Exp -> Ps.NString -> ExpQ
lowerString antiMap = \case
  NixDoubleQuotesString _ parts -> [|Syn.mkDoubleQuotedString $(lowerStringPartList antiMap parts)|]
  NixDoubleSingleQuotesString _ parts -> [|Syn.mkIndentedNString $(lowerStringPartList antiMap parts)|]

lowerStringPart :: IntMap Exp -> NixStringPart Ps.Ps -> ExpQ
lowerStringPart antiMap = \case
  NixStringLiteral _ text -> [|Syn.mkStringLiteral $(liftText text)|]
  NixStringInterpol _ expr -> [|Syn.mkInterpol $(lowerExpr antiMap expr)|]

lowerPath :: IntMap Exp -> Ps.Path -> ExpQ
lowerPath antiMap = \case
  NixLiteralPath _ text -> [|Syn.mkLiteralPath $(liftText text)|]
  NixInterpolPath _ parts -> [|Syn.mkInterpolatedPath $(lowerStringPartList antiMap parts)|]

lowerAttrKey :: IntMap Exp -> Ps.AttrKey -> ExpQ
lowerAttrKey antiMap = \case
  NixStaticAttrKey _ name -> [|Syn.mkAttr $(liftText (unLoc name))|]
  NixDynamicStringAttrKey _ parts -> [|Syn.mkDynamicAttr $(lowerStringPartList antiMap parts)|]
  NixDynamicInterpolAttrKey _ expr -> [|Syn.mkInterpolatedAttr $(lowerExpr antiMap expr)|]

lowerAttrPath :: IntMap Exp -> Ps.AttrPath -> ExpQ
lowerAttrPath antiMap (NixAttrPath _ keys) = [|Syn.mkAttrPath $(lowerAttrKeyList antiMap keys)|]

lowerBinding :: IntMap Exp -> Ps.Binding -> ExpQ
lowerBinding antiMap = \case
  NixNormalBinding _ path expr -> [|Syn.mkNormalBinding $(lowerAttrPath antiMap (unLoc path)) $(lowerExpr antiMap expr)|]
  NixInheritBinding _ mScope keys ->
    lowerInheritBinding antiMap mScope keys

lowerFuncPat :: IntMap Exp -> Ps.FuncPat -> ExpQ
lowerFuncPat antiMap = \case
  NixVarPat _ name -> [|Syn.mkVarPat $(liftText (unLoc name))|]
  NixSetPat _ ellipses mAs bindings ->
    [|Syn.mkSetPat $(lowerSetPatEllipses ellipses) $(lowerMaybe (lowerSetPatAs antiMap . unLoc) mAs) $(lowerSetPatBindingList antiMap bindings)|]

lowerInheritBinding :: IntMap Exp -> Maybe (Located Ps.Expr) -> [Located Ps.AttrKey] -> ExpQ
lowerInheritBinding antiMap mScope keys =
  case mScope of
    Nothing -> [|Syn.mkInheritKeys $(lowerAttrKeyList antiMap keys)|]
    Just scope -> [|Syn.mkInheritFromKeys $(lowerExpr antiMap scope) $(lowerAttrKeyList antiMap keys)|]

lowerSetPatAs :: IntMap Exp -> Ps.SetPatAs -> ExpQ
lowerSetPatAs _ NixSetPatAs {nspaLocation = asLocation, nspaVar = asVar} = [|Syn.mkSetPatAs $(lowerSetPatAsLocation asLocation) $(liftText (unLoc asVar))|]

lowerSetPatBinding :: IntMap Exp -> Ps.SetPatBinding -> ExpQ
lowerSetPatBinding antiMap NixSetPatBinding {nspbVar = bindVar, nspbDefault = bindDefault} = [|Syn.mkSetPatBinding $(liftText (unLoc bindVar)) $(lowerMaybe (lowerExpr antiMap) bindDefault)|]

lowerMaybe :: (a -> ExpQ) -> Maybe a -> ExpQ
lowerMaybe f = \case
  Nothing -> [|Nothing|]
  Just x -> [|Just $(f x)|]

lowerLocatedList :: IntMap Exp -> [Located Ps.Expr] -> ExpQ
lowerLocatedList antiMap = listE . fmap (lowerExpr antiMap)

lowerBindingList :: IntMap Exp -> [Located Ps.Binding] -> ExpQ
lowerBindingList antiMap = listE . fmap (lowerBinding antiMap . unLoc)

lowerStringPartList :: IntMap Exp -> [Located (NixStringPart Ps.Ps)] -> ExpQ
lowerStringPartList antiMap = listE . fmap (lowerStringPart antiMap . unLoc)

lowerAttrKeyList :: IntMap Exp -> [Located Ps.AttrKey] -> ExpQ
lowerAttrKeyList antiMap = listE . fmap (lowerAttrKey antiMap . unLoc)

lowerSetPatBindingList :: IntMap Exp -> [Located Ps.SetPatBinding] -> ExpQ
lowerSetPatBindingList antiMap = listE . fmap (lowerSetPatBinding antiMap . unLoc)

lowerBinaryOp :: BinaryOp -> ExpQ
lowerBinaryOp = \case
  OpConcat -> conE 'OpConcat
  OpMul -> conE 'OpMul
  OpDiv -> conE 'OpDiv
  OpAdd -> conE 'OpAdd
  OpSub -> conE 'OpSub
  OpUpdate -> conE 'OpUpdate
  OpLT -> conE 'OpLT
  OpLE -> conE 'OpLE
  OpGT -> conE 'OpGT
  OpGE -> conE 'OpGE
  OpEq -> conE 'OpEq
  OpNEq -> conE 'OpNEq
  OpAnd -> conE 'OpAnd
  OpOr -> conE 'OpOr
  OpImpl -> conE 'OpImpl

lowerSetPatEllipses :: NixSetPatEllipses -> ExpQ
lowerSetPatEllipses = \case
  NixSetPatIsEllipses -> conE 'NixSetPatIsEllipses
  NixSetPatNotEllipses -> conE 'NixSetPatNotEllipses

lowerSetPatAsLocation :: NixSetPatAsLocation -> ExpQ
lowerSetPatAsLocation = \case
  NixSetPatAsLeading -> conE 'NixSetPatAsLeading
  NixSetPatAsTrailing -> conE 'NixSetPatAsTrailing

liftText :: T.Text -> ExpQ
liftText = litE . StringL . T.unpack
