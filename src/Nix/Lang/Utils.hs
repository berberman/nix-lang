module Nix.Lang.Utils where

import Data.Maybe (catMaybes)
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

showToken :: Token -> Maybe Text
showToken = \case
  TkDoubleSingleQuotes -> Just "''"
  TkDoubleQuote -> Just "\""
  TkAssert -> Just "assert"
  TkIf -> Just "if"
  TkElse -> Just "else"
  TkThen -> Just "then"
  TkLet -> Just "let"
  TkIn -> Just "in"
  TkInherit -> Just "inherit"
  TkRec -> Just "rec"
  TkWith -> Just "with"
  TkOpenC -> Just "{"
  TkCloseC -> Just "}"
  TkOpenS -> Just "["
  TkCloseS -> Just "]"
  TkOpenP -> Just "("
  TkCloseP -> Just ")"
  TkAssign -> Just "="
  TkAt -> Just "@"
  TkColon -> Just ":"
  TkComma -> Just ","
  TkDot -> Just "."
  TkEllipsis -> Just "..."
  TkQuestion -> Just "?"
  TkSemicolon -> Just ";"
  TkConcat -> Just "++"
  TkUpdate -> Just "//"
  TkEx -> Just "!"
  TkAdd -> Just "+"
  TkSub -> Just "-"
  TkMul -> Just "*"
  TkDiv -> Just "/"
  TkAnd -> Just "&&"
  TkOr -> Just "||"
  TkImpl -> Just "->"
  TkEqual -> Just "=="
  TkNEqual -> Just "!="
  TkGT -> Just ">"
  TkGE -> Just ">="
  TkLT -> Just "<"
  TkLE -> Just "<="
  TkId -> Nothing
  TkVal -> Nothing
  TkInterpolOpen -> Just "${"
  TkInterpolClose -> Just "}"
  TkEnvPathOpen -> Just "<"
  TkEnvPathClose -> Just ">"
  TkNeg -> Just "-"
  TkEof -> Nothing

-- >>> tokenString
-- "assertifelsethenletininheritrecwith{}[]()=@:,....?;++//!+-*/&&||->==!=>>=<<=${}<>-''\""
tokenString :: String
tokenString = T.unpack . T.concat . catMaybes $ showToken <$> [TkAssert ..]
