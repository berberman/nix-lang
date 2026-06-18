{-# LANGUAGE TemplateHaskell #-}

module Nix.Lang.QQ (nixQQ) where

import Data.Char (isSpace)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (liftData)
import Nix.Lang.Parser
import Text.Megaparsec (errorBundlePretty)

nixQQ :: QuasiQuoter
nixQQ =
  QuasiQuoter
    { quoteExp = nixQuot,
      quotePat = const $ error "unsupported quotePat",
      quoteType = const $ error "unsupported quoteType",
      quoteDec = const $ error "unsupported quoteDec"
    }

nixQuot :: String -> ExpQ
nixQuot src = do
  let parsed = runNixParser (located nixFile) "<qq>" $ T.pack $ stripCommonPrefix src
  case parsed of
    (Right expr, _) -> liftData expr
    (Left err, _) -> fail $ errorBundlePretty err

stripCommonPrefix :: String -> String
stripCommonPrefix src = unlines $ drop c <$> l
  where
    l = lines src
    c = minimum $ length . takeWhile isSpace <$> filter (any $ not . isSpace) l
