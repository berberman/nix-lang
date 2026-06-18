{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Lang.Parser where

import Control.Monad (forM, guard, msum, unless, void, when)
import Control.Monad.Combinators.Expr
import Control.Monad.State.Strict
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Data (Data)
import Data.Maybe (fromJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Nix.Lang.Annotation
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Text.Megaparsec hiding (State, Token, token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------

data PState = PState
  { psCommentQueue :: [Located Comment],
    psLoc :: (Int, Int)
  }
  deriving (Show, Eq, Data)

type Parser = ParsecT Void Text (State PState)

mkPState :: PState
mkPState = PState [] (1, 1)

runNixParser :: Parser a -> String -> Text -> (Either (ParseErrorBundle Text Void) a, PState)
runNixParser p f t = runState (runParserT p f t) mkPState

--------------------------------------------------------------------------------

takeCommentsBefore :: SrcSpan -> Parser [Located Comment]
takeCommentsBefore s = state $ \ps@PState {..} ->
  let (inside, outside) = foldr step ([], []) psCommentQueue
      step c@(L l _) (ins, outs)
        | commentEndsBefore l s = (c : ins, outs)
        | otherwise = (ins, c : outs)
   in (reverse inside, ps {psCommentQueue = outside})

takeRemainingComments :: Parser [Located Comment]
takeRemainingComments = state $ \ps@PState {..} ->
  (reverse psCommentQueue, ps {psCommentQueue = []})

mkAnnCommon :: SrcSpan -> Parser AnnCommon
mkAnnCommon l = do
  comments <- takeCommentsBefore l
  pure $ AnnCommon (NodeComments comments []) (AnnSpan l)

mkEmptyAnnCommon :: SrcSpan -> AnnCommon
mkEmptyAnnCommon l = AnnCommon emptyComments (AnnSpan l)

addPendingComment ::
  Located Comment ->
  Parser ()
addPendingComment c = modify' $ \ps ->
  ps
    { psCommentQueue = c : psCommentQueue ps
    }

commentEndsBefore :: SrcSpan -> SrcSpan -> Bool
commentEndsBefore comment anchor
  | srcSpanFilename comment /= srcSpanFilename anchor = False
  | otherwise =
      (srcSpanEndLine comment, srcSpanEndColumn comment)
        <= (srcSpanStartLine anchor, srcSpanStartColumn anchor)

attachFollowingComments :: (HasAnnCommon a) => [Located Comment] -> a -> a
attachFollowingComments comments ann
  | null comments = ann
  | otherwise =
      let common = getAnnCommon ann
          nodeComments = acComments common
          common' =
            common
              { acComments =
                  nodeComments
                    { followingComments = followingComments nodeComments <> comments
                    }
              }
       in setAnnCommon common' ann

attachCommentsBeforeAnchor :: (HasAnnCommon a) => SrcSpan -> a -> Parser a
attachCommentsBeforeAnchor anchor ann = do
  comments <- takeCommentsBefore anchor
  pure $ attachFollowingComments comments ann

{-# INLINE sourcePosToLoc #-}
sourcePosToLoc :: SourcePos -> (Int, Int)
sourcePosToLoc pos = (unPos (sourceLine pos), unPos (sourceColumn pos))

putLoc :: (Int, Int) -> Parser ()
putLoc loc = modify' $ \ps -> ps {psLoc = loc}

-- | Invariant: the use of located without consuming whitespace must manually updateLoc
updateLoc :: Parser ()
updateLoc = getSourcePos >>= putLoc . sourcePosToLoc

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
  when allocateComments $ pure ()
  pure $ L s x

located :: Parser a -> Parser (Located a)
located = located' False False

locatedC :: Parser a -> Parser (Located a)
locatedC = located

eatLineComment :: Parser ()
eatLineComment = do
  c <- located'
    True
    False
    $ do
      _ <- string "#"
      takeWhileP (Just "chars") (\x -> x /= '\n' && x /= '\r')
  addPendingComment $ LineComment <$> c

eatBlockComment :: Parser ()
eatBlockComment = do
  c <- located' True False $ do
    _ <- string "/*"
    content <- manyTill anySingle $ string "*/"
    pure $ T.pack content
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
  (L l (_, x, _)) <-
    locatedC $
      (,,)
        <$> located (token t1 includeWhitespaceOpen <* unless includeWhitespaceOpen updateLoc)
        <*> p
        <*> located (token t2 includeWhitespaceClose <* unless includeWhitespaceClose updateLoc)
  pure $ L l x

antiquote :: Bool -> Bool -> Parser a -> Parser (Located a)
antiquote = betweenToken AnnInterpolOpen AnnInterpolClose

legalReserved :: Parser ()
legalReserved =
  lookAhead $
    void eof
      <|> void
        ( satisfy $
            \x -> not $ isIdentChar x || isPathChar x
        )

reservedKw :: Text -> Parser (Located Text)
reservedKw x = try $ located $ lexeme $ symbol' x <* legalReserved

--------------------------------------------------------------------------------

litBoolean :: Parser (NixExpr Ps)
litBoolean = litTrue <|> litFalse
  where
    litTrue = do
      (L l _) <- reservedKw "true"
      common <- mkAnnCommon l
      pure $ NixLit common $ L l $ NixBoolean NoExtF True
    litFalse = do
      (L l _) <- reservedKw "false"
      common <- mkAnnCommon l
      pure $ NixLit common $ L l $ NixBoolean NoExtF False

litNull :: Parser (NixExpr Ps)
litNull = do
  (L l _) <- reservedKw "null"
  common <- mkAnnCommon l
  pure $ NixLit common $ L l $ NixNull NoExtF

litFloat :: Parser (NixExpr Ps)
litFloat = do
  (L l f) <- located $ lexeme L.float
  common <- mkAnnCommon l
  pure $ NixLit common $ L l $ NixFloat NoExtF f

litInteger :: Parser (NixExpr Ps)
litInteger = do
  (L l f) <- located $ lexeme L.decimal
  common <- mkAnnCommon l
  pure $ NixLit common $ L l $ NixInteger NoExtF f

litUri :: Parser (NixExpr Ps)
litUri = do
  (L l uri) <- located $ lexeme $ do
    h <- letterChar
    scheme <- takeWhileP (Just "scheme") isSchemeChar
    colon <- char ':'
    rest <- takeWhile1P (Just "uri") isUriChar
    pure $ T.cons h scheme <> T.cons colon rest
  common <- mkAnnCommon l
  pure $ NixLit common $ L l $ NixUri NoExtF uri

--------------------------------------------------------------------------------
slash :: Parser Char
slash = char '/' <* notFollowedBy (satisfy $ \x -> x == '/' || isSpace x || x == '>')

nixEnvPath :: Parser (NixExpr Ps)
nixEnvPath =
  lexeme $
    betweenToken AnnEnvPathOpen AnnEnvPathClose False False (lookAhead (satisfy (/= '/')) >> T.pack <$> many (satisfy isPathChar <|> slash)) >>= \(L l p) -> do
      common <- mkAnnCommon l
      let ann = AnnEnvPathNode common (parsedAnnToken AnnEnvPathOpen l) (parsedAnnToken AnnEnvPathClose l)
      pure $ NixEnvPath ann (L l p)

literalPath :: Parser (NixExpr Ps)
literalPath =
  locatedC
    ( NixLiteralPath NoExtF
        <$> ( do
                u <- takeWhileP (Just "path") isPathChar
                r <- some (T.cons <$> slash <*> takeWhile1P (Just "path") isPathChar)
                pure $ T.concat $ u : r
            )
        <* updateLoc
    )
    >>= \(L l p) -> do
      common <- mkAnnCommon l
      pure $ NixPath (AnnPathNode common) (L l p)

interpolPath :: Parser (NixExpr Ps)
interpolPath =
  located path >>= \(L l p) -> do
    let parts = mergeStringPartLiteral p
    guard $ slashInFirstPart parts
    common <- mkAnnCommon l
    pure $ NixPath (AnnPathNode common) $ L l $ NixInterpolPath NoExtF parts
  where
    -- at least one slash before interpolation
    slashInFirstPart (L _ (NixStringLiteral _ s) : _) = T.elem '/' s
    slashInFirstPart _ = False
    lit = located $ NixStringLiteral NoExtF . T.pack <$> some (notFollowedBy (char '$') >> satisfy isPathChar) <* updateLoc
    interpol = located nixStringPartInterpol
    slash' = located $ NixStringLiteral NoExtF . T.singleton <$> slash <* updateLoc
    path = (<>) <$> (maybeToList <$> optional (lit <|> interpol)) <*> (some (lit <|> interpol <|> slash') <* notFollowedBy slash')

nixPath :: Parser (NixExpr Ps)
nixPath = lexeme $ try (literalPath <* notFollowedBy "$") <|> (interpolPath <* notFollowedBy "*")

pathStartsHere :: Parser Bool
pathStartsHere =
  option False . try . lookAhead $ do
    _ <- takeWhile1P (Just "path prefix") isPathChar
    void slash
    pure True

uriStartsHere :: Parser Bool
uriStartsHere =
  option False . try . lookAhead $ do
    _ <- letterChar
    _ <- takeWhileP (Just "scheme") isSchemeChar
    _ <- char ':'
    _ <- takeWhile1P (Just "uri") isUriChar
    pure True

--------------------------------------------------------------------------------
ident :: Parser Text
ident = try $
  lexeme $ do
    _ <- lookAhead (satisfy (\x -> isAlpha x || x == '_'))
    x <- takeWhile1P (Just "ident") isIdentChar
    when (x `elem` reservedNames) $
      fail $
        "'" <> T.unpack x <> "' is a reserved name"
    pure x

nixVar :: Parser (NixExpr Ps)
nixVar =
  located ident >>= \(L l x) -> do
    common <- mkAnnCommon l
    pure $ NixVar common (L l x)

--------------------------------------------------------------------------------

escapedChars :: [(Char, Char)]
escapedChars =
  [ ('n', '\n'),
    ('t', '\t'),
    ('r', '\r'),
    ('\\', '\\'),
    ('$', '$'),
    ('"', '"'),
    ('\'', '\''),
    ('.', '.'),
    ('{', '{')
  ]

escapedChar :: Parser Char
escapedChar = choice [char code >> pure r | (code, r) <- escapedChars] <|> anySingle

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
                <$> escapeStart
                <*> anySingle
            )
            <|> T.singleton
            <$> (notFollowedBy (end <|> escapeStart) >> anySingle)

doubleQuotesString :: Parser (NixExpr Ps)
doubleQuotesString =
  betweenToken AnnDoubleQuote AnnDoubleQuote False False lit >>= \(L l s) -> do
    common <- mkAnnCommon l
    pure $ NixString (AnnStringNode common) (L l s)
  where
    lit = do
      (src, parsedParts) <- match doubleQuotedStringParts
      pure $ NixDoubleQuotesString (SourceText src) (mergeStringPartLiteral parsedParts)

doubleQuotedStringParts :: Parser [LNixStringPart Ps]
doubleQuotedStringParts = many (located $ ((NixStringLiteral NoExtF <$> string "$$") <|> nixStringPartInterpol <|> nixStringPartLiteral "\"" "\\" doubleQuotedStringEscape) <* updateLoc)

doubleQuotedStringEscape :: Parser (NixStringPart Ps)
doubleQuotedStringEscape = NixStringLiteral NoExtF . T.singleton <$> (char '\\' >> escapedChar)

doubleSingleQuotesString :: Parser (NixExpr Ps)
doubleSingleQuotesString = s >>= expr
  where
    -- we can't use stringSourceText here since '' can escape ''
    s =
      lookAhead $
        between (string "''") (string "''") $
          fmap (SourceText . T.concat) $
            many $
              msum
                [ try $ T.snoc <$> string "''" <*> (char '$' <|> char '\''),
                  notFollowedBy ("''" <* notFollowedBy "\\") >> T.singleton <$> anySingle
                ]
    escape =
      try $
        NixStringLiteral NoExtF
          <$> ( string "''"
                  >> ( ((char '\'' >> pure "''") <|> (char '$' >> pure "$"))
                         <|> (char '\\' >> (T.singleton <$> escapedChar))
                     )
              )
    -- @$${@ does not indicate an interpolation, so we try to consume $$ first
    parts = many (located $ ((NixStringLiteral NoExtF <$> string "$$") <|> nixStringPartInterpol <|> nixStringPartLiteral "''" "''" escape) <* updateLoc)
    lit src = fmap (NixDoubleSingleQuotesString src . mergeStringPartLiteral) parts
    expr src =
      betweenToken AnnDoubleSingleQuotes AnnDoubleSingleQuotes False False (lit src) >>= \(L l str) -> do
        common <- mkAnnCommon l
        pure $ NixString (AnnStringNode common) (L l str)

nixString :: Parser (NixExpr Ps)
nixString = lexeme $ doubleQuotesString <|> doubleSingleQuotesString

--------------------------------------------------------------------------------

nixPar :: Parser (NixExpr Ps)
nixPar =
  locatedC ((,,) <$> open <*> located nixExpr <*> close) >>= \(L l (lp, x, rp)) -> do
    common <- mkAnnCommon l
    ann <- attachCommentsBeforeAnchor (getLoc rp) $ AnnParNode common (parsedAnnToken AnnOpenP (getLoc lp)) (parsedAnnToken AnnCloseP (getLoc rp))
    pure $ NixPar ann x
  where
    open = located (symbol' "(" <* updateLoc) <* ws
    close = located (symbol' ")" <* updateLoc) <* ws

--------------------------------------------------------------------------------

dot :: Parser Text
dot = try $ lexeme $ token AnnDot False <* notFollowedBy ("/" <|> ".")

attrKey :: Parser (NixAttrKey Ps)
attrKey = dynamicString <|> dynamicInterpol <|> static
  where
    static = NixStaticAttrKey NoExtF <$> located ident
    dynamicString =
      lexeme (betweenToken AnnDoubleQuote AnnDoubleQuote False False (mergeStringPartLiteral <$> doubleQuotedStringParts)) >>= \(L _ x) ->
        pure $ NixDynamicStringAttrKey NoExtF x
    dynamicInterpol =
      NixDynamicInterpolAttrKey NoExtF
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
      common <- mkAnnCommon l
      pure $ NixAttrPath (AnnAttrPath common (parsedAnnToken AnnDot <$> ld)) a

--------------------------------------------------------------------------------

inherit :: Parser (NixBinding Ps)
inherit =
  locatedC ((,,,) <$> kw <*> set <*> keys <*> end) >>= \(L l (a, b, c, d)) -> do
    common <- mkAnnCommon l
    let ann = AnnInheritBinding common (parsedAnnToken AnnInherit (getLoc a)) (parsedAnnToken AnnSemicolon (getLoc d))
    pure $ NixInheritBinding ann b c
  where
    kw = try $ located $ symbol' "inherit" <* (legalReserved >> ws)
    set = optional $ located nixPar
    keys = many $ located attrKey
    end = located $ symbol ";"

normal :: Parser (NixBinding Ps)
normal =
  locatedC ((,,,) <$> path <*> eq <*> expr <*> end) >>= \(L l (a, b, c, d)) -> do
    common <- mkAnnCommon l
    let ann = AnnNormalBinding common (parsedAnnToken AnnAssign (getLoc b)) (parsedAnnToken AnnSemicolon (getLoc d))
    pure $ NixNormalBinding ann a c
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
    common <- mkAnnCommon l
    ann <-
      attachCommentsBeforeAnchor (getLoc d) $
        AnnSet common (parsedAnnToken AnnRec . getLoc <$> a) (parsedAnnToken AnnOpenC (getLoc b)) (parsedAnnToken AnnCloseC (getLoc d))
    pure $ NixSet ann (maybe NixSetNonRecursive (const NixSetRecursive) a) c
  where
    kw = optional $ reservedKw "rec"
    open = located (symbol' "{" <* updateLoc) <* ws
    close = located (symbol' "}" <* updateLoc) <* ws
    bindings = located' True False $ many $ located nixBinding <* updateLoc

--------------------------------------------------------------------------------

varPat :: Parser (NixFuncPat Ps)
varPat =
  (try litUri >> fail "unexpected uri")
    <|> ( located ident >>= \li -> do
            let l = getLoc li
            common <- mkAnnCommon l
            let ann = AnnVarPat common (getLoc li)
            pure $ NixVarPat ann li
        )

-- Note: this is not identical to @pat $ try bodyWithLeading <|> bodyWithTrailing@
setPat :: Parser (NixFuncPat Ps)
setPat = try (pat bodyWithLeading) <|> pat bodyWithTrailing
  where
    leadingAs :: Parser (Located (Located (), NixSetPatAs Ps))
    leadingAs = do
      L l (i, a) <- located $ (,) <$> located ident <*> located (void $ symbol "@")
      common <- mkAnnCommon l
      pure $ L l (a, NixSetPatAs (AnnSetPatAs common (parsedAnnToken AnnAt (getLoc a))) NixSetPatAsLeading i)
    trailingAs :: Parser (Located (Located (), NixSetPatAs Ps))
    trailingAs = do
      L l (a, i) <- located $ (,) <$> located (void $ symbol "@") <*> located ident
      common <- mkAnnCommon l
      pure $ L l (a, NixSetPatAs (AnnSetPatAs common (parsedAnnToken AnnAt (getLoc a))) NixSetPatAsTrailing i)
    bind = located $ (,) <$> located ident <*> optional ((,) <$> located (void $ symbol "?") <*> located nixExpr)
    ellipsis = located $ void $ symbol "..."
    openBrace = located (symbol' "{" <* updateLoc) <* ws
    closeBrace = located (symbol' "}" <* updateLoc) <* ws
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
    body ::
      Parser
        ( Located
            ( Located Text,
              ( [Located (Located Text, Maybe (Located (), LNixExpr Ps))],
                [Located ()],
                Maybe (Located ())
              ),
              Located Text
            )
        )
    body = locatedC $ (,,) <$> openBrace <*> go mempty <*> closeBrace
    bodyWithLeading = (,) <$> (Just <$> leadingAs) <*> body
    bodyWithTrailing = flip (,) <$> body <*> optional trailingAs
    pat ::
      -- (asPattern, [bind], [comma], ellipsis)
      Parser
        ( Maybe (Located (Located (), NixSetPatAs Ps)),
          Located
            ( Located Text,
              ( [Located (Located Text, Maybe (Located (), LNixExpr Ps))],
                [Located ()],
                Maybe (Located ())
              ),
              Located Text
            )
        ) ->
      Parser (NixFuncPat Ps)
    pat x = do
      (L l (mas, L _lBody (lo, (bs, cs, me), lcBody))) <- located x

      -- as pattern
      ras <- case mas of
        Just (L al (_, as)) -> do
          pure $ Just $ L al as
        Nothing -> pure Nothing

      -- ellipsis
      re <- case me of
        Just _ -> pure NixSetPatIsEllipses
        _ -> pure NixSetPatNotEllipses

      -- bindings
      rb <- forM bs $ \(L lb (i, mDefault)) -> do
        (rQuestion, rDef) <- case mDefault of
          Just (q, def) -> pure (Just (parsedAnnToken AnnQuestion (getLoc q)), Just def)
          _ -> pure (Nothing, Nothing)
        commonBinding <- mkAnnCommon lb
        pure $ L lb $ NixSetPatBinding (AnnSetPatBinding commonBinding rQuestion) i rDef

      common <- mkAnnCommon l
      let ann =
            AnnSetPatNode
              common
              (parsedAnnToken AnnOpenC (getLoc lo))
              (parsedAnnToken AnnCloseC (getLoc lcBody))
              (parsedAnnToken AnnEllipsis . getLoc <$> me)
              (parsedAnnToken AnnComma . getLoc <$> cs)
      pure $ NixSetPat ann re ras rb

nixFuncPat :: Parser (NixFuncPat Ps)
nixFuncPat = try setPat <|> varPat

nixLam :: Parser (NixExpr Ps)
nixLam = try $ do
  pat <- located nixFuncPat
  colon <- located (symbol ":")
  body <- located nixExpr
  let l = getLoc pat `combineSrcSpans` getLoc body
  common <- mkAnnCommon l
  pure $ NixLam (AnnLamNode common (parsedAnnToken AnnColon (getLoc colon))) pat body

--------------------------------------------------------------------------------

nixList :: Parser (NixExpr Ps)
nixList =
  locatedC ((,,) <$> open <*> many (located nixTerm) <*> close) >>= \(L l (ls, xs, rs)) -> do
    common <- mkAnnCommon l
    ann <- attachCommentsBeforeAnchor (getLoc rs) $ AnnListNode common (parsedAnnToken AnnOpenS (getLoc ls)) (parsedAnnToken AnnCloseS (getLoc rs))
    pure $ NixList ann xs
  where
    open = located (symbol' "[" <* updateLoc) <* ws
    close = located (symbol' "]" <* updateLoc) <* ws

--------------------------------------------------------------------------------

nixIf :: Parser (NixExpr Ps)
nixIf =
  body >>= \(L l (kif, op, kth, e1, kel, e2)) -> do
    common <- mkAnnCommon l
    let ann = AnnIfNode common (parsedAnnToken AnnIf (getLoc kif)) (parsedAnnToken AnnThen (getLoc kth)) (parsedAnnToken AnnElse (getLoc kel))
    pure $ NixIf ann op e1 e2
  where
    kwIf = reservedKw "if"
    kwThen = reservedKw "then"
    kwElse = reservedKw "else"
    body = located $ (,,,,,) <$> kwIf <*> located nixExpr <*> kwThen <*> located nixExpr <*> kwElse <*> located nixExpr

--------------------------------------------------------------------------------

nixWith :: Parser (NixExpr Ps)
nixWith =
  body >>= \(L l (kwi, e1, semicolon, e2)) -> do
    common <- mkAnnCommon l
    let ann = AnnWithNode common (parsedAnnToken AnnWith (getLoc kwi)) (parsedAnnToken AnnSemicolon (getLoc semicolon))
    pure $ NixWith ann e1 e2
  where
    kw = reservedKw "with"
    sem = located $ symbol ";"
    body = located $ (,,,) <$> kw <*> located nixExpr <*> sem <*> located nixExpr

--------------------------------------------------------------------------------

nixAssert :: Parser (NixExpr Ps)
nixAssert =
  body >>= \(L l (kas, e1, semicolon, e2)) -> do
    common <- mkAnnCommon l
    let ann = AnnAssertNode common (parsedAnnToken AnnAssert (getLoc kas)) (parsedAnnToken AnnSemicolon (getLoc semicolon))
    pure $ NixAssert ann e1 e2
  where
    ka = reservedKw "assert"
    sem = located $ symbol ";"
    body = located $ (,,,) <$> ka <*> located nixExpr <*> sem <*> located nixExpr

--------------------------------------------------------------------------------

nixLet :: Parser (NixExpr Ps)
nixLet =
  body >>= \(L l (kl, bs, ki, e)) -> do
    common <- mkAnnCommon l
    let ann = AnnLetNode common (parsedAnnToken AnnLet (getLoc kl)) (parsedAnnToken AnnIn (getLoc ki))
    pure $ NixLet ann bs e
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
    '/' -> try nixPath
    '<' -> try nixEnvPath
    '\"' -> nixString
    '\'' -> nixString
    '.' -> nixPath
    '~' -> nixPath
    '_' -> do
      isPath <- pathStartsHere
      if isPath then try nixPath <|> nixVar else nixVar
    x
      | isDigit x -> do
          isPath <- pathStartsHere
          if isPath
            then nixPath
            else try litFloat <|> litInteger
      | isAlpha x -> do
          isPath <- pathStartsHere
          if isPath
            then if x == 'r' then try nixSet <|> try nixPath <|> alphaLike x else try nixPath <|> alphaLike x
            else alphaLike x
    _ -> fail "unexpected token"
  where
    alphaLike x
      | x == 'r' = try nixSet <|> keywordOrUriOrVar x
      | otherwise = keywordOrUriOrVar x

    keywordOrUriOrVar x
      | x == 'n' = litNull <|> uriOrVar
      | x == 't' || x == 'f' = litBoolean <|> uriOrVar
      | otherwise = uriOrVar

    uriOrVar = do
      isUri <- uriStartsHere
      if isUri then litUri else nixVar

-- | Term is term' with selection
nixTerm :: Parser (NixExpr Ps)
nixTerm = do
  t <- located nixTerm'
  ms <- optional $ try $ located $ attrPath True
  case ms of
    (Just s) ->
      optional
        (located $ (,) <$> kwOr <*> def)
        >>= \case
          Just (L bl (o, d)) -> do
            common <- mkAnnCommon (getLoc s `combineSrcSpans` bl)
            let ann = AnnSelect common (Just (parsedAnnToken AnnOr (getLoc o)))
            pure $ NixSelect ann t s $ Just d
          _ -> do
            common <- mkAnnCommon (getLoc t `combineSrcSpans` getLoc s)
            let ann = AnnSelect common Nothing
            pure $ NixSelect ann t s Nothing
    Nothing -> pure $ unLoc t
  where
    kwOr = reservedKw "or"
    def = located nixExpr

--------------------------------------------------------------------------------

operator :: Ann -> Parser (Located Ann)
operator t = try $ located $ lexeme $ t <$ symbol' (fromJust $ showToken t) <* notFollowedBy (oneOf opString)

type OpParser = Operator Parser (LNixExpr Ps)

appOp :: OpParser
appOp = InfixL $ pure $ \e1 e2 ->
  let l = getLoc e1 `combineSrcSpans` getLoc e2
   in L l $ NixApp (AnnAppNode (mkEmptyAnnCommon l)) e1 e2

notAppOp :: OpParser
notAppOp =
  Prefix $
    ( \(L lt _) e ->
        let l = lt `combineSrcSpans` getLoc e
            ann = AnnPrefixNode (mkEmptyAnnCommon l) (parsedAnnToken AnnEx lt)
         in L l $ NixNotApp ann e
    )
      <$> operator AnnEx

negAppOp :: OpParser
negAppOp =
  Prefix $
    ( \(L lt _) e ->
        let l = lt `combineSrcSpans` getLoc e
            ann = AnnPrefixNode (mkEmptyAnnCommon l) (parsedAnnToken AnnNeg lt)
         in L l $ NixNegApp ann e
    )
      <$> operator AnnNeg

hasAttrOp :: OpParser
hasAttrOp =
  Postfix $
    ( \(L lt _) p e ->
        let l = lt `combineSrcSpans` getLoc p `combineSrcSpans` getLoc e
            ann = AnnHasAttr (mkEmptyAnnCommon l) (parsedAnnToken AnnQuestion lt)
         in L l $ NixHasAttr ann e p
    )
      <$> operator AnnQuestion
      <*> located (attrPath False)

infixOp ::
  ( Parser
      ( Located (NixExpr Ps) ->
        Located (NixExpr Ps) ->
        Located (NixExpr Ps)
      ) ->
    OpParser
  ) ->
  BinaryOp ->
  OpParser
infixOp f op =
  f $
    ( \(L lt _) e1 e2 ->
        let l = getLoc e1 `combineSrcSpans` lt `combineSrcSpans` getLoc e2
            ann = AnnBinAppNode (mkEmptyAnnCommon l) (parsedAnnToken (opToToken op) lt)
         in L l $ NixBinApp ann op e1 e2
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
    [infixR OpUpdate],
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
nixOp = unLoc <$> makeExprParser (located nixTerm) opTable

--------------------------------------------------------------------------------

nixExpr :: Parser (NixExpr Ps)
nixExpr = nixLet <|> nixIf <|> nixAssert <|> nixWith <|> nixLam <|> nixOp

--------------------------------------------------------------------------------

nixFile :: Parser (NixExpr Ps)
nixFile = do
  ws
  expr <- nixExpr
  trailing <- takeRemainingComments
  eof
  pure $ attachRootTrailingComments trailing expr

attachRootTrailingComments :: [Located Comment] -> NixExpr Ps -> NixExpr Ps
attachRootTrailingComments comments = \case
  NixVar ann name -> NixVar (attachFollowingComments comments ann) name
  NixLit ann lit -> NixLit (attachFollowingComments comments ann) lit
  NixPar ann expr -> NixPar (attachFollowingComments comments ann) expr
  NixString ann str -> NixString (attachFollowingComments comments ann) str
  NixPath ann path -> NixPath (attachFollowingComments comments ann) path
  NixEnvPath ann path -> NixEnvPath (attachFollowingComments comments ann) path
  NixLam ann pat body -> NixLam (attachFollowingComments comments ann) pat body
  NixApp ann fun arg -> NixApp (attachFollowingComments comments ann) fun arg
  NixBinApp ann op lhs rhs -> NixBinApp (attachFollowingComments comments ann) op lhs rhs
  NixNotApp ann expr -> NixNotApp (attachFollowingComments comments ann) expr
  NixNegApp ann expr -> NixNegApp (attachFollowingComments comments ann) expr
  NixList ann xs -> NixList (attachFollowingComments comments ann) xs
  NixSet ann kind bindings -> NixSet (attachFollowingComments comments ann) kind bindings
  NixLet ann bindings expr -> NixLet (attachFollowingComments comments ann) bindings expr
  NixHasAttr ann expr path -> NixHasAttr (attachFollowingComments comments ann) expr path
  NixSelect ann expr path def -> NixSelect (attachFollowingComments comments ann) expr path def
  NixIf ann cond thenExpr elseExpr -> NixIf (attachFollowingComments comments ann) cond thenExpr elseExpr
  NixWith ann scope expr -> NixWith (attachFollowingComments comments ann) scope expr
  NixAssert ann assertion expr -> NixAssert (attachFollowingComments comments ann) assertion expr
