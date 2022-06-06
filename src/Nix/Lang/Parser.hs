{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Lang.Parser where

import Control.Monad.State.Strict
import Data.Char (isDigit, isLower, isSpace, isUpper)
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

--------------------------------------------------------------------------------

located' :: Bool -> Parser a -> Parser (Located a)
located' includeWhiteSpace p = do
  start <- getSourcePos
  x <- p
  beforeWs <- gets psLoc
  when (beforeWs < sourcePosToLoc start) $
    getSourcePos >>= putLoc . sourcePosToLoc
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
    allocatePendingComments s {srcSpanEndLine = l, srcSpanEndColumn = c}
  pure $ L s x

allocatePendingComments :: SrcSpan -> Parser ()
allocatePendingComments s = modify' $ \ps@PState {..} ->
  let (before, rest) = break (\(L l _) -> l `isSubspanOf` s) psPendingComments
      (middle, after) = break (\(L l _) -> not $ l `isSubspanOf` s) rest
      pending = before ++ after
      comments = [(s, middle) | not (null middle)]
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
ws = do
  getSourcePos >>= putLoc . sourcePosToLoc
  L.space space1 eatLineComment eatBlockComment

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
isIdentChar x = isUpper x || isLower x || isDigit x || x `elem` ['-', '_', '\'']

isPathChar :: Char -> Bool
isPathChar x = isUpper x || isLower x || x `elem` ['.', '_', '-', '+', '~']

--------------------------------------------------------------------------------

annVal :: Parser (Located a) -> Parser (Located a)
annVal p = do
  x <- p
  let l = getLoc x
  addAnnotation l TkVal l
  pure x

betweenToken :: Token -> Token -> Bool -> Parser a -> Parser (Located a)
betweenToken t1 t2 includeWhitespaces p = do
  (L l (open, x, close)) <-
    located $
      (,,) <$> located (token t1 includeWhitespaces)
        <*> p
        <*> located (token t2 includeWhitespaces)
  addAnnotation l t1 (getLoc open)
  addAnnotation l t2 (getLoc close)
  pure $ L l x

parnes :: Parser a -> Parser (Located a)
parnes = betweenToken TkOpenP TkCloseP True

braces :: Parser a -> Parser (Located a)
braces = betweenToken TkOpenC TkCloseC True

brackets :: Parser a -> Parser (Located a)
brackets = betweenToken TkOpenS TkCloseS True

angles :: Parser a -> Parser (Located a)
angles = betweenToken TkLT TkGT False

antiquote :: Parser a -> Parser (Located a)
antiquote = betweenToken TkInterpolOpen TkCloseC False

--------------------------------------------------------------------------------

float :: Parser (NixExpr Ps)
float = do
  (L l f) <- annVal $ located $ lexeme L.float
  pure $ NixLit NoExtF $ L l $ NixFloat NoExtF f

integer :: Parser (NixExpr Ps)
integer = do
  (L l f) <- annVal $ located $ lexeme L.decimal
  addAnnotation l TkVal l
  pure $ NixLit NoExtF $ L l $ NixInteger NoExtF f

--------------------------------------------------------------------------------
slash :: Parser Char
slash = char '/' <* notFollowedBy (satisfy $ \x -> x == '/' || isSpace x || x == '>')

envPath :: Parser (NixExpr Ps)
envPath =
  lexeme $
    fmap (NixEnvPath NoExtF) $
      angles $ do
        lookAhead (satisfy (/= '/')) >> T.pack <$> many (satisfy isPathChar <|> slash)

--------------------------------------------------------------------------------
