{-# LANGUAGE TemplateHaskell #-}

-- | Quasiquoter for embedding Nix syntax in Haskell code.
--
-- 'nixQQ' is the convenient way to keep small Nix snippets inline in Haskell
-- code, especially in tests. It parses at compile time, so broken snippets fail
-- where they are written rather than later at runtime.
--
-- Example:
--
-- @
-- {-# LANGUAGE QuasiQuotes #-}
--
-- import Nix.Lang.QQ (nixQQ)
--
-- expr = [nixQQ| let x = 1; in x |]
-- @
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
