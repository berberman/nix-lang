-- | Shared lexical character classes and string escaping helpers.
--
-- Parser and printer code both rely on this module so that identifier, path, and
-- string rules stay in sync.
module Nix.Lang.Lexer.Text
  ( escapedChars,
    escapeDoubleQuotedText,
    escapeIndentedText,
    reservedNames,
    isIdentChar,
    isPathChar,
    isSchemeChar,
    isUriChar,
  ) where

import Data.Char (isAlpha, isDigit)
import Data.Text (Text)
import qualified Data.Text as T

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

escapeDoubleQuotedText :: Text -> Text
escapeDoubleQuotedText = foldr (.) id [T.replace (T.singleton char) (T.cons '\\' (T.singleton code)) | (code, char) <- escapedChars]

escapeIndentedText :: Text -> Text
escapeIndentedText = foldr (.) id [T.replace "${" "''${", T.replace "''" "'''"]

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
