module Nix.Lang.Utils where

import Data.Char (isAlpha, isDigit)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Types

mkSrcSpan :: String -> (Int, Int) -> (Int, Int) -> SrcSpan
mkSrcSpan
  srcSpanFilename
  (srcSpanStartLine, srcSpanStartColumn)
  (srcSpanEndLine, srcSpanEndColumn)
    | (srcSpanEndLine, srcSpanEndColumn) >= (srcSpanStartLine, srcSpanStartColumn) = SrcSpan {..}
    | otherwise = error $ show (srcSpanEndLine, srcSpanEndColumn) <> " is less than " <> show (srcSpanStartLine, srcSpanStartColumn)

isSubspanOf ::
  SrcSpan ->
  SrcSpan ->
  Bool
isSubspanOf src parent
  | srcSpanFilename parent /= srcSpanFilename src = False
  | otherwise =
      (srcSpanStartLine parent, srcSpanStartColumn parent) <= (srcSpanStartLine src, srcSpanStartColumn src)
        && (srcSpanEndLine parent, srcSpanEndLine parent) >= (srcSpanEndLine src, srcSpanEndLine src)

combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans span1 span2 = SrcSpan f sl sc el ec
  where
    (sl, sc) =
      min
        (srcSpanStartLine span1, srcSpanStartColumn span1)
        (srcSpanStartLine span2, srcSpanStartColumn span2)
    (el, ec) =
      max
        (srcSpanEndLine span1, srcSpanEndColumn span1)
        (srcSpanEndLine span2, srcSpanEndColumn span2)
    f = srcSpanFilename span1

unLoc :: Located a -> a
unLoc (L _ x) = x

getLoc :: Located a -> SrcSpan
getLoc (L s _) = s

showToken :: Ann -> Maybe Text
showToken = \case
  AnnDoubleSingleQuotes -> Just "''"
  AnnDoubleQuote -> Just "\""
  AnnAssert -> Just "assert"
  AnnIf -> Just "if"
  AnnElse -> Just "else"
  AnnThen -> Just "then"
  AnnLet -> Just "let"
  AnnIn -> Just "in"
  AnnInherit -> Just "inherit"
  AnnRec -> Just "rec"
  AnnWith -> Just "with"
  AnnOpenC -> Just "{"
  AnnCloseC -> Just "}"
  AnnOpenS -> Just "["
  AnnCloseS -> Just "]"
  AnnOpenP -> Just "("
  AnnCloseP -> Just ")"
  AnnAssign -> Just "="
  AnnAt -> Just "@"
  AnnColon -> Just ":"
  AnnComma -> Just ","
  AnnDot -> Just "."
  AnnEllipsis -> Just "..."
  AnnQuestion -> Just "?"
  AnnSemicolon -> Just ";"
  AnnConcat -> Just "++"
  AnnUpdate -> Just "//"
  AnnEx -> Just "!"
  AnnAdd -> Just "+"
  AnnSub -> Just "-"
  AnnMul -> Just "*"
  AnnDiv -> Just "/"
  AnnAnd -> Just "&&"
  AnnOr -> Just "||"
  AnnImpl -> Just "->"
  AnnEqual -> Just "=="
  AnnNEqual -> Just "!="
  AnnGT -> Just ">"
  AnnGE -> Just ">="
  AnnLT -> Just "<"
  AnnLE -> Just "<="
  AnnId -> Nothing
  AnnVal -> Nothing
  AnnInterpolOpen -> Just "${"
  AnnInterpolClose -> Just "}"
  AnnEnvPathOpen -> Just "<"
  AnnEnvPathClose -> Just ">"
  AnnNeg -> Just "-"
  AnnEof -> Nothing

opToToken :: BinaryOp -> Ann
opToToken = \case
  OpConcat -> AnnConcat
  OpMul -> AnnMul
  OpDiv -> AnnDiv
  OpAdd -> AnnAdd
  OpSub -> AnnSub
  OpUpdate -> AnnUpdate
  OpLT -> AnnLT
  OpLE -> AnnLE
  OpGT -> AnnGT
  OpGE -> AnnGE
  OpEq -> AnnEqual
  OpNEq -> AnnNEqual
  OpAnd -> AnnAnd
  OpOr -> AnnOr
  OpImpl -> AnnImpl

showBinOP :: BinaryOp -> Text
showBinOP = fromJust . showToken . opToToken

-- >>> opString
-- "++*/+-//<<=>>===!=&&||->"
opString :: String
opString = T.unpack . T.concat $ showBinOP <$> [OpConcat ..]

reservedNames :: [Text]
reservedNames = ["rec", "let", "in", "with", "inherit", "assert", "if", "then", "else"]

isIdentChar :: Char -> Bool
isIdentChar x = isAlpha x || isDigit x || x `elem` ['-', '_', '\'']

isPathChar :: Char -> Bool
isPathChar x = isAlpha x || isDigit x || x `elem` ['.', '_', '-', '+', '~']

isSchemeChar :: Char -> Bool
isSchemeChar x = isAlpha x || isDigit x || x `elem` ['+', '-', '.']

isUriChar :: Char -> Bool
isUriChar x = isAlpha x || isDigit x || x `elem` ['~', '!', '@', '$', '%', '&', '*', '-', '=', '_', '+', ':', ',', '.', '/', '?']
