{-# LANGUAGE TemplateHaskell #-}

module Nix.Lang.QQ.Parsed (nixParsedQQ) where

import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Nix.Lang.Parser (located, nixFile, runNixParser)
import Nix.Lang.Utils (stripCommonPrefix)
import Text.Megaparsec (errorBundlePretty)

nixParsedQQ :: QuasiQuoter
nixParsedQQ =
  QuasiQuoter
    { quoteExp = nixParsedQuot,
      quotePat = const $ error "nixParsedQQ: unsupported quotePat",
      quoteType = const $ error "nixParsedQQ: unsupported quoteType",
      quoteDec = const $ error "nixParsedQQ: unsupported quoteDec"
    }

nixParsedQuot :: String -> ExpQ
nixParsedQuot src =
  case runNixParser (located nixFile) "<qq>" (T.pack (stripCommonPrefix src)) of
    (Left err, _) -> fail (errorBundlePretty err)
    (Right expr, _) -> dataToExpQ (const Nothing) expr
