{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Lang.Parser where

import Control.Monad.Combinators.Expr
import Control.Monad.State.Strict
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Data (Data)
import Data.Maybe (fromJust, maybeToList)
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
    psAnnotation :: [AddAnn],
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
    { psAnnotation = AddAnn lt a ls : psAnnotation ps
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
collectComment = fmap unLoc . locatedC

--------------------------------------------------------------------------------

located' :: Bool -> Bool -> Parser a -> Parser (Located a)
located' includeWhiteSpace allocateComments p = do
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
  when allocateComments $ do
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
located = located' False False

locatedC :: Parser a -> Parser (Located a)
locatedC = located' False True

eatLineComment :: Parser ()
eatLineComment = do
  c <- located'
    True
    False
    $ do
      pound <- string "#"
      (pound <>) <$> takeWhileP (Just "chars") (\x -> x /= '\n' && x /= '\r')
  addPendingComment $ LineComment <$> c

eatBlockComment :: Parser ()
eatBlockComment = do
  c <- located' True False $ do
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

--------------------------------------------------------------------------------

betweenToken :: Ann -> Ann -> Bool -> Bool -> Parser a -> Parser (Located a)
betweenToken t1 t2 includeWhitespaceOpen includeWhitespaceClose p = do
  (L l (open, x, close)) <-
    locatedC $
      (,,) <$> located (token t1 includeWhitespaceOpen <* unless includeWhitespaceOpen updateLoc)
        <*> p
        <*> located (token t2 includeWhitespaceClose <* unless includeWhitespaceClose updateLoc)
  addAnnotation l t1 (getLoc open)
  addAnnotation l t2 (getLoc close)
  pure $ L l x

antiquote :: Bool -> Bool -> Parser a -> Parser (Located a)
antiquote = betweenToken AnnInterpolOpen AnnInterpolClose

legalReserved :: Parser ()
legalReserved = lookAhead $
  void $
    satisfy $ \x -> isSpace x || (x `notElem` T.unpack (T.concat reservedNames) && x `elem` tokenString)

reservedKw :: Text -> Parser (Located Text)
reservedKw x = located $ lexeme $ symbol' x <* legalReserved

--------------------------------------------------------------------------------

litBoolean :: Parser (NixExpr Ps)
litBoolean = litTrue <|> litFalse
  where
    litTrue = do
      (L l _) <- reservedKw "true"
      pure $ NixLit NoExtF $ L l $ NixBoolean NoExtF True
    litFalse = do
      (L l _) <- reservedKw "false"
      pure $ NixLit NoExtF $ L l $ NixBoolean NoExtF False

litNull :: Parser (NixExpr Ps)
litNull = do
  (L l _) <- reservedKw "null"
  pure $ NixLit NoExtF $ L l $ NixNull NoExtF

litFloat :: Parser (NixExpr Ps)
litFloat = do
  (L l f) <- located $ lexeme L.float
  pure $ NixLit NoExtF $ L l $ NixFloat NoExtF f

litInteger :: Parser (NixExpr Ps)
litInteger = do
  (L l f) <- located $ lexeme L.decimal
  pure $ NixLit NoExtF $ L l $ NixInteger NoExtF f

litUri :: Parser (NixExpr Ps)
litUri = fmap (NixLit NoExtF) $
  located $
    lexeme $ do
      h <- letterChar
      scheme <- takeWhileP (Just "scheme") isSchemeChar
      colon <- char ':'
      uri <- takeWhile1P (Just "uri") isUriChar
      pure $ NixUri NoExtF $ T.cons h scheme <> T.cons colon uri

--------------------------------------------------------------------------------
slash :: Parser Char
slash = char '/' <* notFollowedBy (satisfy $ \x -> x == '/' || isSpace x || x == '>')

nixEnvPath :: Parser (NixExpr Ps)
nixEnvPath =
  collectComment $
    lexeme $
      fmap (NixEnvPath NoExtF) $
        betweenToken AnnEnvPathOpen AnnEnvPathClose False False $ do
          lookAhead (satisfy (/= '/')) >> T.pack <$> many (satisfy isPathChar <|> slash)

literalPath :: Parser (NixExpr Ps)
literalPath =
  fmap (NixPath NoExtF) $
    locatedC $
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
    pure $ NixPath NoExtF $ L l $ NixInterpolPath (SourceText $ T.take delta i) $ mergeStringPartLiteral p
  where
    mkList a b = [a, b]
    lit = located $ NixStringLiteral NoExtF . T.pack <$> some (notFollowedBy (char '$') >> satisfy isPathChar) <* updateLoc
    slash' = located $ NixStringLiteral NoExtF . T.singleton <$> slash <* updateLoc
    interpol = located nixStringPartInterpol
    path = (<>) <$> (maybeToList <$> optional (lit <|> interpol)) <*> (concat <$> some (mkList <$> slash' <*> (lit <|> interpol)))

nixPath :: Parser (NixExpr Ps)
nixPath = collectComment $ lexeme $ try literalPath <|> interpolPath

--------------------------------------------------------------------------------
ident :: Parser Text
ident = try $
  lexeme $ do
    _ <- lookAhead (satisfy (\x -> isAlpha x || x == '_'))
    x <- takeWhile1P (Just "ident") isIdentChar
    when (x `elem` reservedNames) $
      fail $ "'" <> T.unpack x <> "' is a reserved name"
    pure x

nixVar :: Parser (NixExpr Ps)
nixVar = NixVar NoExtF <$> located ident

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
nixStringPartInterpol = NixStringInterpol NoExtF <$> antiquote True False nixExpr

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
    expr src = fmap (NixString NoExtF) $ betweenToken AnnDoubleQuote AnnDoubleQuote False False $ lit src

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
    expr src = fmap (NixString NoExtF) $ betweenToken AnnDoubleSingleQuotes AnnDoubleSingleQuotes False False $ lit src

nixString :: Parser (NixExpr Ps)
nixString = collectComment $ lexeme $ doubleQuotesString <|> doubleSingleQuotesString

--------------------------------------------------------------------------------

nixPar :: Parser (NixExpr Ps)
nixPar = NixPar NoExtF <$> betweenToken AnnOpenP AnnCloseP True True nixExpr

--------------------------------------------------------------------------------

dot :: Parser Text
dot = lexeme $ token AnnDot False <* notFollowedBy nixPath

attrKey :: Parser (NixAttrKey Ps)
attrKey = dynamicString <|> dynamicInterpol <|> static
  where
    static = NixStaticAttrKey NoExtF <$> located ident
    dynamicString =
      lexeme doubleQuotesString >>= \case
        (NixString _ (L _ (NixDoubleQuotesString src x))) -> pure $ NixDynamicStringAttrKey src x
        _ -> fail "Impossible"
    dynamicInterpol =
      stringSourceText "${" "}" empty >>= \src ->
        NixDynamicInterpolAttrKey src
          <$> antiquote True True nixExpr

attrPath :: Bool -> Parser (NixAttrPath Ps)
attrPath dotFirst =
  locatedC
    ( do
        mdot <- if dotFirst then pure <$> located dot else pure []
        h <- located attrKey
        (rd, ra) <- fmap unzip $ many $ (,) <$> located dot <*> located attrKey
        pure (fmap getLoc $ mdot <> rd, h : ra)
    )
    >>= \(L l (ld, a)) -> do
      forM_ ld $ addAnnotation l AnnDot
      pure $ NixAttrPath a

--------------------------------------------------------------------------------

inherit :: Parser (NixBinding Ps)
inherit =
  locatedC ((,,,) <$> kw <*> set <*> keys <*> end) >>= \(L l (a, b, c, d)) -> do
    addAnnotation l AnnInherit $ getLoc a
    addAnnotation l AnnSemicolon $ getLoc d
    pure $ NixInheritBinding NoExtF b c
  where
    kw = located $ symbol' "inherit" <* (legalReserved >> ws)
    set = optional $ located nixPar
    keys = many $ located attrKey
    end = located $ symbol ";"

normal :: Parser (NixBinding Ps)
normal =
  locatedC ((,,,) <$> path <*> eq <*> expr <*> end) >>= \(L l (a, b, c, d)) -> do
    addAnnotation l AnnEqual $ getLoc b
    addAnnotation l AnnSemicolon $ getLoc d
    pure $ NixNormalBinding NoExtF a c
  where
    path = located $ attrPath False
    eq = located $ symbol "="
    expr = located nixExpr
    end = located $ symbol ";"

nixBinding :: Parser (NixBinding Ps)
nixBinding = inherit <|> normal

--------------------------------------------------------------------------------

nixSet :: Parser (NixExpr Ps)
nixSet =
  locatedC ((,,,) <$> kw <*> open <*> bindings <*> close) >>= \(L l (a, b, c, d)) -> do
    maybe (pure ()) (addAnnotation l AnnRec . getLoc) a
    addAnnotation l AnnOpenC $ getLoc b
    addAnnotation l AnnCloseC $ getLoc d
    pure $ NixSet NoExtF (maybe NixSetNonRecursive (const NixSetRecursive) a) c
  where
    -- TODO: why do we need these updateLoc?
    kw = optional $ reservedKw "rec"
    open = located $ symbol "{" <* updateLoc
    close = located $ symbol "}"
    bindings = located $ many $ located nixBinding <* updateLoc

--------------------------------------------------------------------------------

varPat :: Parser (NixFuncPat Ps)
varPat =
  (try litUri >> fail "unexpected uri")
    <|> ( located ((,) <$> located ident <*> located (symbol ":")) >>= \(L l (li, lc)) -> do
            addAnnotation l AnnId $ getLoc li
            addAnnotation l AnnColon $ getLoc lc
            pure $ NixVarPat NoExtF li
        )

-- Note: this is not identical to @pat $ try bodyWithLeading <|> pat bodyWithTrailing@
setPat :: Parser (NixFuncPat Ps)
setPat = try (pat bodyWithLeading) <|> pat bodyWithTrailing
  where
    leadingAs = located $ (\i a -> (a, NixSetPatAs NixSetPatAsLeading i)) <$> located ident <*> located (void $ symbol "@")
    trailingAs = located $ (\a i -> (a, NixSetPatAs NixSetPatAsTrailing i)) <$> located (void $ symbol "@") <*> located ident
    bind = located $ (,) <$> located ident <*> optional ((,) <$> located (void $ symbol "?") <*> located nixExpr)
    ellipsis = located $ void $ symbol "..."
    -- ([bind], [comma])
    go ::
      ([Located (Located Text, Maybe (Located (), LNixExpr Ps))], [Located ()]) ->
      -- ([bind], [comma], ellipsis)
      Parser ([Located (Located Text, Maybe (Located (), LNixExpr Ps))], [Located ()], Maybe (Located ()))
    go (bs, cs) = ((bs,cs,) . pure <$> ellipsis) <|> go1
      where
        go1 = option (bs, cs, Nothing) $ do
          b <- bind
          let (bs1, cs1) = (bs <> [b], cs)
          option (bs1, cs1, Nothing) $ do
            c <- located $ void $ symbol ","
            go (bs1, cs1 <> [c])
    -- TODO: maybe it would be better to attach braces to the span of the outermost ast
    body = betweenToken AnnOpenC AnnCloseC True True $ go mempty
    bodyWithLeading = (,) <$> optional leadingAs <*> body
    bodyWithTrailing = flip (,) <$> body <*> optional trailingAs
    pat ::
      -- (asPattern, [bind], [comma], ellipsis)
      Parser
        ( Maybe (Located (Located (), NixSetPatAs Ps)),
          Located
            ( [Located (Located Text, Maybe (Located (), LNixExpr Ps))],
              [Located ()],
              Maybe (Located ())
            )
        ) ->
      Parser (NixFuncPat Ps)
    pat x = do
      (L l ((mas, L lBody (bs, cs, me)), L lc _)) <- located $ (,) <$> x <*> located (symbol ":")

      -- as pattern
      ras <- case mas of
        Just (L al (aal, as)) -> do
          -- add as to the span of the pattern
          addAnnotation al AnnAt $ getLoc aal
          pure $ Just $ L al as
        Nothing -> pure Nothing

      -- commas
      forM_ (zip (getLoc <$> bs) (getLoc <$> cs)) $ \(bl, cl) ->
        -- add commas to the span of each term
        addAnnotation bl AnnComma cl

      -- ellipsis
      re <- case me of
        Just (L el _) -> do
          -- add ellipsis to the span of the braces
          addAnnotation lBody AnnEllipsis el
          pure NixSetPatIsEllipses
        _ -> pure NixSetPatNotEllipses

      -- bindings
      rb <- forM bs $ \(L lb (i, mDefault)) -> do
        rDef <- case mDefault of
          Just (q, def) -> do
            -- add question mark to the span of each binding
            addAnnotation lb AnnQuestion $ getLoc q
            pure $ Just def
          _ -> pure Nothing
        addAnnotation lb AnnId $ getLoc i
        pure $ L lb $ NixSetPatBinding i rDef

      -- colon after the pattern
      addAnnotation l AnnColon lc

      pure $ NixSetPat NoExtF re ras rb

nixFuncPat :: Parser (NixFuncPat Ps)
nixFuncPat = try varPat <|> setPat

nixLam :: Parser (NixExpr Ps)
nixLam = collectComment $ NixLam NoExtF <$> try (located nixFuncPat) <*> located nixExpr

--------------------------------------------------------------------------------

nixList :: Parser (NixExpr Ps)
nixList =
  fmap (NixList NoExtF . unLoc) $
    betweenToken AnnOpenS AnnCloseS True True $ many $ located nixTerm

--------------------------------------------------------------------------------

nixIf :: Parser (NixExpr Ps)
nixIf =
  collectComment $
    body >>= \(L l (kif, op, kth, e1, kel, e2)) -> do
      addAnnotation l AnnIf $ getLoc kif
      addAnnotation l AnnThen $ getLoc kth
      addAnnotation l AnnElse $ getLoc kel
      pure $ NixIf NoExtF op e1 e2
  where
    kwIf = reservedKw "if"
    kwThen = reservedKw "then"
    kwElse = reservedKw "else"
    body = located $ (,,,,,) <$> kwIf <*> located nixOp <*> kwThen <*> located nixExpr <*> kwElse <*> located nixExpr

--------------------------------------------------------------------------------

nixWith :: Parser (NixExpr Ps)
nixWith =
  collectComment $
    body >>= \(L l (kwi, e1, semicolon, e2)) -> do
      addAnnotation l AnnWith $ getLoc kwi
      addAnnotation l AnnSemicolon $ getLoc semicolon
      pure $ NixWith NoExtF e1 e2
  where
    kw = reservedKw "with"
    sem = located $ symbol ";"
    body = located $ (,,,) <$> kw <*> located nixExpr <*> sem <*> located nixExpr

--------------------------------------------------------------------------------

nixAssert :: Parser (NixExpr Ps)
nixAssert =
  collectComment $
    body >>= \(L l (kas, e1, semicolon, e2)) -> do
      addAnnotation l AnnAssert $ getLoc kas
      addAnnotation l AnnSemicolon $ getLoc semicolon
      pure $ NixWith NoExtF e1 e2
  where
    ka = reservedKw "assert"
    sem = located $ symbol ";"
    body = located $ (,,,) <$> ka <*> located nixExpr <*> sem <*> located nixExpr

--------------------------------------------------------------------------------

nixLet :: Parser (NixExpr Ps)
nixLet =
  collectComment $
    body >>= \(L l (kl, bs, ki, e)) -> do
      addAnnotation l AnnLet $ getLoc kl
      addAnnotation l AnnIn $ getLoc ki
      pure $ NixLet NoExtF bs e
  where
    kLet = reservedKw "let"
    kIn = reservedKw "in"
    bindings = located $ many $ located nixBinding
    body = located $ (,,,) <$> kLet <*> bindings <*> kIn <*> located nixExpr

--------------------------------------------------------------------------------

-- | Term' is expr without operators (including selection)
nixTerm' :: Parser (NixExpr Ps)
nixTerm' =
  lookAhead anySingle >>= \case
    '(' -> nixPar
    '{' -> nixSet
    '[' -> nixList
    '/' -> nixPath
    '<' -> nixEnvPath
    '\"' -> nixString
    '\'' -> nixString
    x ->
      msum $
        [nixSet | x == 'r']
          <> [try nixPath | isPathChar x]
          <> [try litFloat <|> litInteger | isDigit x]
          <> [litNull | x == 'n']
          <> [litBoolean | x == 't' || x == 'f']
          <> [try litUri, nixVar]

-- | Term is term' with selection
nixTerm :: Parser (NixExpr Ps)
nixTerm = do
  t <- located nixTerm'
  ms <- optional $ located $ attrPath True
  case ms of
    (Just s) ->
      optional
        ( located $ (,) <$> kwOr <*> def
        )
        >>= \case
          Just (L bl (o, d)) -> do
            addAnnotation (getLoc s `combineSrcSpans` bl) AnnOr $ getLoc o
            pure $ NixSelect NoExtF t s $ Just d
          _ -> pure $ NixSelect NoExtF t s Nothing
    Nothing -> pure $ unLoc t
  where
    kwOr = reservedKw "or"
    def = located nixExpr

--------------------------------------------------------------------------------

operator :: Ann -> Parser (Located Ann)
operator t = try $ located $ lexeme $ t <$ symbol' (fromJust $ showToken t) <* notFollowedBy (oneOf opString)

type OpParser = Operator Parser ([AddAnn], LNixExpr Ps)

appOp :: OpParser
appOp = InfixL $ pure $ \(a1, e1) (a2, e2) -> (a1 <> a2, L (getLoc e1 `combineSrcSpans` getLoc e2) $ NixApp NoExtF e1 e2)

notAppOp :: OpParser
notAppOp =
  Prefix $
    ( \(L lt t) (a, e) ->
        let l = lt `combineSrcSpans` getLoc e
         in (AddAnn l t lt : a, L l $ NixNotApp NoExtF e)
    )
      <$> operator AnnEx

negAppOp :: OpParser
negAppOp =
  Prefix $
    ( \(L lt t) (a, e) ->
        let l = lt `combineSrcSpans` getLoc e
         in (AddAnn l t lt : a, L l $ NixNegApp NoExtF e)
    )
      <$> operator AnnNeg

hasAttrOp :: OpParser
hasAttrOp =
  Postfix $
    ( \(L lt t) p (a, e) ->
        let l = lt `combineSrcSpans` getLoc p `combineSrcSpans` getLoc e
         in (AddAnn l t lt : a, L l $ NixHasAttr NoExtF e p)
    )
      <$> operator AnnQuestion
      <*> located (attrPath False)

infixOp ::
  ( Parser
      ( ([AddAnn], Located (NixExpr Ps)) ->
        ([AddAnn], Located (NixExpr Ps)) ->
        ([AddAnn], Located (NixExpr Ps))
      ) ->
    OpParser
  ) ->
  BinaryOp ->
  OpParser
infixOp f op =
  f $
    ( \(L lt t) (a1, e1) (a2, e2) ->
        let l = getLoc e1 `combineSrcSpans` lt `combineSrcSpans` getLoc e2
         in (AddAnn l t lt : a2 <> a1, L l $ NixBinApp NoExtF op e1 e2)
    )
      <$> operator (opToToken op)

opTable :: [[OpParser]]
opTable =
  [ [appOp],
    [negAppOp],
    [hasAttrOp],
    [infixR OpConcat],
    [infixL OpMul, infixL OpDiv],
    [infixL OpAdd, infixL OpSub],
    [notAppOp],
    [infixOp InfixR OpUpdate],
    [infixL OpLT, infixL OpLE, infixL OpGT, infixL OpGE],
    [infixN OpEq, infixN OpNEq],
    [infixL OpAnd],
    [infixL OpOr],
    [infixL OpImpl]
  ]
  where
    infixL = infixOp InfixL
    infixN = infixOp InfixN
    infixR = infixOp InfixR

nixOp :: Parser (NixExpr Ps)
nixOp = do
  (a, r) <- makeExprParser (([],) <$> located nixTerm) opTable
  modify' $ \ps -> ps {psAnnotation = a <> psAnnotation ps}
  pure $ unLoc r

--------------------------------------------------------------------------------

nixExpr :: Parser (NixExpr Ps)
nixExpr = nixLet <|> nixIf <|> nixAssert <|> nixWith <|> nixLam <|> nixOp

--------------------------------------------------------------------------------
