{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Lang.Parser where

import Control.Monad.State.Strict
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Data (Data)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Nix.Lang.Types
import Nix.Lang.Utils
import Text.Megaparsec hiding (State, Token, token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------

data PState = PState
  { psPendingComments :: [Located Comment],
    psComments :: [(SrcSpan, [Located Comment])],
    psAnnotation :: [(SrcSpan, Token, SrcSpan)],
    psLoc :: (Int, Int)
  }
  deriving (Show, Eq, Data)

type Parser = ParsecT Void Text (State PState)

--------------------------------------------------------------------------------

addAnnotation ::
  -- | Smallest AST the annotation belongs to
  SrcSpan ->
  -- | Annotation
  Token ->
  -- | Span of the annotation
  SrcSpan ->
  Parser ()
addAnnotation lt a ls = modify' $ \ps ->
  ps
    { psAnnotation = (lt, a, ls) : psAnnotation ps
    }

addPendingComment ::
  Located Comment ->
  Parser ()
addPendingComment c = modify' $ \ps ->
  ps
    { psPendingComments = c : psPendingComments ps
    }

sourcePosToLoc :: SourcePos -> (Int, Int)
sourcePosToLoc pos = (unPos (sourceLine pos), unPos (sourceColumn pos))

putLoc :: (Int, Int) -> Parser ()
putLoc loc = modify' $ \ps -> ps {psLoc = loc}

-- | Invariant: the use of located without consuming whitespaces must manually updateLoc
updateLoc :: Parser ()
updateLoc = getSourcePos >>= putLoc . sourcePosToLoc

collectComment :: Parser a -> Parser a
collectComment = fmap unLoc . located

--------------------------------------------------------------------------------

located' :: Bool -> Parser a -> Parser (Located a)
located' includeWhiteSpace p = do
  start <- getSourcePos
  x <- p
  end <-
    if not includeWhiteSpace
      then gets psLoc
      else sourcePosToLoc <$> getSourcePos
  let s =
        mkSrcSpan
          (sourceName start)
          (unPos $ sourceLine start, unPos $ sourceColumn start)
          end
  unless includeWhiteSpace $ do
    (l, c) <- sourcePosToLoc <$> getSourcePos
    allocatePendingComments s {srcSpanEndLine = l, srcSpanEndColumn = c} s
  pure $ L s x

allocatePendingComments ::
  -- | Span of ast extended to whitespaces
  SrcSpan ->
  -- | Span of ast
  SrcSpan ->
  Parser ()
allocatePendingComments s s' = modify' $ \ps@PState {..} ->
  let (before, rest) = break (\(L l _) -> l `isSubspanOf` s) psPendingComments
      (middle, after) = break (\(L l _) -> not $ l `isSubspanOf` s) rest
      pending = before ++ after
      comments = [(s', middle) | not (null middle)]
   in ps
        { psPendingComments = pending,
          psComments = comments <> psComments
        }

located :: Parser a -> Parser (Located a)
located = located' False

eatLineComment :: Parser ()
eatLineComment = do
  c <- located'
    True
    $ do
      pound <- string "#"
      (pound <>) <$> takeWhileP (Just "chars") (\x -> x /= '\n' && x /= '\r')
  addPendingComment $ LineComment <$> c

eatBlockComment :: Parser ()
eatBlockComment = do
  c <- located' True $ do
    start <- string "/*"
    content <- manyTill anySingle $ string "*/"
    pure $ start <> T.pack content
  addPendingComment $ BlockComment <$> c

ws :: Parser ()
ws = updateLoc >> L.space space1 eatLineComment eatBlockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol = L.symbol ws

-- | Not including whitespaces
symbol' :: Text -> Parser Text
symbol' = string

token :: Token -> Bool -> Parser Text
token tk includeWhitespaces = (if includeWhitespaces then symbol else symbol') $ fromJust $ showToken tk

reservedNames :: [Text]
reservedNames = ["rec", "let", "in", "with", "inherit", "assert", "if", "then", "else"]

isIdentChar :: Char -> Bool
isIdentChar x = isAlpha x || isDigit x || x `elem` ['-', '_', '\'']

isPathChar :: Char -> Bool
isPathChar x = isAlpha x || isDigit x || x `elem` ['.', '_', '-', '+', '~']

--------------------------------------------------------------------------------

annSingle :: Token -> Parser (Located a) -> Parser (Located a)
annSingle tk p = do
  x <- p
  let l = getLoc x
  addAnnotation l tk l
  pure x

annVal :: Parser (Located a) -> Parser (Located a)
annVal = annSingle TkVal

annId :: Parser (Located a) -> Parser (Located a)
annId = annSingle TkId

betweenToken :: Token -> Token -> Bool -> Parser a -> Parser (Located a)
betweenToken t1 t2 includeWhitespaces p = do
  (L l (open, x, close)) <-
    located $
      (,,) <$> located (token t1 includeWhitespaces <* unless includeWhitespaces updateLoc)
        <*> p
        <*> located (token t2 includeWhitespaces <* unless includeWhitespaces updateLoc)
  addAnnotation l t1 (getLoc open)
  addAnnotation l t2 (getLoc close)
  pure $ L l x

parnes :: Parser a -> Parser (Located a)
parnes = betweenToken TkOpenP TkCloseP True

braces :: Parser a -> Parser (Located a)
braces = betweenToken TkOpenC TkCloseC True

brackets :: Parser a -> Parser (Located a)
brackets = betweenToken TkOpenS TkCloseS True

antiquote :: Parser a -> Parser (Located a)
antiquote = betweenToken TkInterpolOpen TkInterpolClose False

legalReserved :: Parser ()
legalReserved = lookAhead $
  void $
    satisfy $ \x -> isSpace x || (x `notElem` T.unpack (T.concat reservedNames) && x `elem` tokenString)

--------------------------------------------------------------------------------

litBoolean :: Parser (NixExpr Ps)
litBoolean = litTrue <|> litFalse
  where
    litTrue = do
      (L l _) <- annVal $ located $ lexeme $ symbol' "true" <* legalReserved
      pure $ NixLit NoExtF $ L l $ NixBoolean NoExtF True
    litFalse = do
      (L l _) <- annVal $ located $ lexeme $ symbol' "false" <* legalReserved
      pure $ NixLit NoExtF $ L l $ NixBoolean NoExtF False

litNull :: Parser (NixExpr Ps)
litNull = do
  (L l _) <- annVal $ located $ lexeme $ symbol' "null" <* legalReserved
  pure $ NixLit NoExtF $ L l $ NixNull NoExtF

litFloat :: Parser (NixExpr Ps)
litFloat = do
  (L l f) <- annVal $ located $ lexeme L.float
  pure $ NixLit NoExtF $ L l $ NixFloat NoExtF f

litInteger :: Parser (NixExpr Ps)
litInteger = do
  (L l f) <- annVal $ located $ lexeme L.decimal
  pure $ NixLit NoExtF $ L l $ NixInteger NoExtF f

--------------------------------------------------------------------------------
slash :: Parser Char
slash = char '/' <* notFollowedBy (satisfy $ \x -> x == '/' || isSpace x || x == '>')

envPath :: Parser (NixExpr Ps)
envPath =
  collectComment $
    lexeme $
      fmap (NixEnvPath NoExtF) $
        betweenToken TkEnvPathOpen TkEnvPathClose False $ do
          lookAhead (satisfy (/= '/')) >> T.pack <$> many (satisfy isPathChar <|> slash)

--------------------------------------------------------------------------------
ident :: Parser (NixExpr Ps)
ident = fmap (NixVar NoExtF) $
  annId $
    located $
      lexeme $ do
        _ <- lookAhead (satisfy (\x -> isAlpha x || x == '_'))
        x <- takeWhile1P (Just "ident") isIdentChar
        when (x `elem` reservedNames) $
          fail $ T.unpack x <> " is a reserved name"
        pure x

--------------------------------------------------------------------------------
escapedChars :: [(Char, Char)]
escapedChars =
  [ ('n', '\n'),
    ('t', '\t'),
    ('r', '\r'),
    ('\\', '\\'),
    ('$', '$'),
    ('"', '"')
  ]

escapedChar :: Parser Char
escapedChar = choice [char code >> pure r | (code, r) <- escapedChars]

mergeStringPartLiteral :: [LNixStringPart Ps] -> [LNixStringPart Ps]
mergeStringPartLiteral [] = []
mergeStringPartLiteral (L l1 (NixStringLiteral _ t1) : L l2 (NixStringLiteral _ t2) : xs) =
  L (l1 `combineSrcSpans` l2) (NixStringLiteral NoExtF $ t1 <> t2) : xs
mergeStringPartLiteral (x : xs) = x : mergeStringPartLiteral xs

nixStringPartLiteral :: Char -> Char -> Parser (NixStringPart Ps) -> Parser (NixStringPart Ps)
nixStringPartLiteral end escapeStart escape =
  (NixStringLiteral NoExtF . T.singleton <$> char '$')
    <|> escape
    <|> (NixStringLiteral NoExtF . T.pack <$> some (notFollowedBy (char end <|> char '$' <|> char escapeStart) >> anySingle))

nixStringPartInterpol :: Parser (NixStringPart Ps)
nixStringPartInterpol = NixStringInterpol NoExtF <$> antiquote nixExpr

doubleQuotesString :: Parser (NixExpr Ps)
doubleQuotesString =
  fmap (NixString NoExtF) $
    betweenToken TkDoubleQuote TkDoubleQuote False $
      fmap (NixDoubleQuotesString NoExtF . mergeStringPartLiteral) $
        many $ located $ (nixStringPartInterpol <|> nixStringPartLiteral '\"' '\\' escape) <* updateLoc
  where
    escape = NixStringLiteral NoExtF . T.singleton <$> (char '\\' >> escapedChar)

nixString :: Parser (NixExpr Ps)
nixString = collectComment $ lexeme doubleQuotesString

nixExpr :: Parser (NixExpr Ps)
nixExpr = litInteger

--------------------------------------------------------------------------------
