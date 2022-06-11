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
    psAnnotation :: [(SrcSpan, Ann, SrcSpan)],
    psLoc :: (Int, Int)
  }
  deriving (Show, Eq, Data)

type Parser = ParsecT Void Text (State PState)

--------------------------------------------------------------------------------

addAnnotation ::
  -- | Smallest AST the annotation belongs to
  SrcSpan ->
  -- | Annotation
  Ann ->
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

-- | Invariant: the use of located without consuming whitespace must manually updateLoc
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
  -- | Span of ast extended to whitespace
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

-- | Not including whitespace
symbol' :: Text -> Parser Text
symbol' = string

token :: Ann -> Bool -> Parser Text
token tk includeWhitespace = (if includeWhitespace then symbol else symbol') $ fromJust $ showToken tk

reservedNames :: [Text]
reservedNames = ["rec", "let", "in", "with", "inherit", "assert", "if", "then", "else"]

isIdentChar :: Char -> Bool
isIdentChar x = isAlpha x || isDigit x || x `elem` ['-', '_', '\'']

isPathChar :: Char -> Bool
isPathChar x = isAlpha x || isDigit x || x `elem` ['.', '_', '-', '+', '~']

--------------------------------------------------------------------------------

annSingle :: Ann -> Parser (Located a) -> Parser (Located a)
annSingle tk p = do
  x <- p
  let l = getLoc x
  addAnnotation l tk l
  pure x

annVal :: Parser (Located a) -> Parser (Located a)
annVal = annSingle AnnVal

annId :: Parser (Located a) -> Parser (Located a)
annId = annSingle AnnId

betweenToken :: Ann -> Ann -> Bool -> Parser a -> Parser (Located a)
betweenToken t1 t2 includeWhitespace p = do
  (L l (open, x, close)) <-
    located $
      (,,) <$> located (token t1 includeWhitespace <* unless includeWhitespace updateLoc)
        <*> p
        <*> located (token t2 includeWhitespace <* unless includeWhitespace updateLoc)
  addAnnotation l t1 (getLoc open)
  addAnnotation l t2 (getLoc close)
  pure $ L l x

braces :: Parser a -> Parser (Located a)
braces = betweenToken AnnOpenC AnnCloseC True

brackets :: Parser a -> Parser (Located a)
brackets = betweenToken AnnOpenS AnnCloseS True

antiquote :: Parser a -> Parser (Located a)
antiquote = betweenToken AnnInterpolOpen AnnInterpolClose False

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
        betweenToken AnnEnvPathOpen AnnEnvPathClose False $ do
          lookAhead (satisfy (/= '/')) >> T.pack <$> many (satisfy isPathChar <|> slash)

literalPath :: Parser (NixExpr Ps)
literalPath =
  fmap (NixPath NoExtF) $
    located $
      NixLiteralPath NoExtF
        <$> ( do
                u <- takeWhileP (Just "path") isPathChar
                r <- some (T.cons <$> slash <*> takeWhile1P (Just "path") isPathChar)
                pure $ T.concat $ u : r
            )
          <* updateLoc

interpolPath :: Parser (NixExpr Ps)
interpolPath =
  -- TODO: the entire input got copied here
  (,,) <$> getInput <*> getOffset <*> located path >>= \(i, o, L l p) -> do
    delta <- (\o' -> o' - o) <$> getOffset
    pure $ NixPath NoExtF $ L l $ NixInterpolPath (SourceText $ T.take delta i) p
  where
    lit = located $ NixStringLiteral NoExtF . T.pack <$> some (notFollowedBy (char '$') >> satisfy isPathChar <|> slash) <* updateLoc
    interpol = located nixStringPartInterpol
    path = many (lit <|> interpol)

nixPath :: Parser (NixExpr Ps)
nixPath = collectComment $ lexeme $ try literalPath <|> interpolPath

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
    ('"', '"'),
    ('\'', '\'')
  ]

escapedChar :: Parser Char
escapedChar = choice [char code >> pure r | (code, r) <- escapedChars]

mergeStringPartLiteral :: [LNixStringPart Ps] -> [LNixStringPart Ps]
mergeStringPartLiteral [] = []
mergeStringPartLiteral (L l1 (NixStringLiteral _ t1) : L l2 (NixStringLiteral _ t2) : xs) =
  mergeStringPartLiteral $ L (l1 `combineSrcSpans` l2) (NixStringLiteral NoExtF $ t1 <> t2) : xs
mergeStringPartLiteral (x : xs) = x : mergeStringPartLiteral xs

nixStringPartLiteral :: Parser Text -> Parser Text -> Parser (NixStringPart Ps) -> Parser (NixStringPart Ps)
nixStringPartLiteral end escapeStart escape =
  (NixStringLiteral NoExtF . T.singleton <$> char '$')
    <|> escape
    <|> (NixStringLiteral NoExtF . T.pack <$> some (notFollowedBy (void end <|> void (char '$') <|> void escapeStart) >> anySingle))

nixStringPartInterpol :: Parser (NixStringPart Ps)
nixStringPartInterpol = NixStringInterpol NoExtF <$> antiquote nixExpr

-- | Get source text without consuming it
stringSourceText :: Parser Text -> Parser Text -> Parser Text -> Parser SourceText
stringSourceText start end escapeStart =
  lookAhead $
    between start end $
      fmap (SourceText . T.concat) $
        many $
          try
            ( (\a b -> a <> T.singleton b)
                <$> escapeStart <*> oneOf (fmap fst escapedChars)
            )
            <|> T.singleton <$> (notFollowedBy (end <|> escapeStart) >> anySingle)

doubleQuotesString :: Parser (NixExpr Ps)
doubleQuotesString = stringSourceText (string "\"") (string "\"") (string "\\") >>= expr
  where
    escape = NixStringLiteral NoExtF . T.singleton <$> (char '\\' >> escapedChar)
    -- @$${@ does not indicate an interpolation, so we try to consume $$ first
    parts = many (located $ ((NixStringLiteral NoExtF <$> string "$$") <|> nixStringPartInterpol <|> nixStringPartLiteral "\"" "\\" escape) <* updateLoc)
    lit src = fmap (NixDoubleQuotesString src . mergeStringPartLiteral) parts
    expr src = fmap (NixString NoExtF) $ betweenToken AnnDoubleQuote AnnDoubleQuote False $ lit src

doubleSingleQuotesString :: Parser (NixExpr Ps)
doubleSingleQuotesString = stringSourceText (string "''") (string "''") (string "''") >>= expr
  where
    escape =
      try $
        NixStringLiteral NoExtF
          <$> ( string "''"
                  >> ( (char '\'' >> pure "''")
                         <|> (T.singleton <$> escapedChar)
                     )
              )
    -- @$${@ does not indicate an interpolation, so we try to consume $$ first
    parts = many (located $ ((NixStringLiteral NoExtF <$> string "$$") <|> nixStringPartInterpol <|> nixStringPartLiteral "''" "''" escape) <* updateLoc)
    lit src = fmap (NixDoubleSingleQuotesString src . mergeStringPartLiteral) parts
    expr src = fmap (NixString NoExtF) $ betweenToken AnnDoubleSingleQuotes AnnDoubleSingleQuotes False $ lit src

nixString :: Parser (NixExpr Ps)
nixString = collectComment $ lexeme $ doubleQuotesString <|> doubleSingleQuotesString

--------------------------------------------------------------------------------

nixPar :: Parser (NixExpr Ps)
nixPar = NixPar NoExtF <$> betweenToken AnnOpenP AnnCloseP True nixExpr

--------------------------------------------------------------------------------

nixTerm :: Parser (NixExpr Ps)
nixTerm = undefined

nixExpr :: Parser (NixExpr Ps)
nixExpr = litInteger

--------------------------------------------------------------------------------
