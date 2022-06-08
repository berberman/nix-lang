module Main (main) where

import Control.Monad.State.Strict ( runState )
import qualified Data.Text.IO as T
import Nix.Lang.Parser
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = test nixString

test :: (Show a) => Parser a -> IO ()
test parser =
  T.readFile "test.nix" >>= \src ->
    let m = runParserT parser "test.nix" src
     in case runState m $ PState [] [] [] (1, 1) of
          (Right x, s) -> pPrint x >> pPrint s
          (Left err, s) -> putStrLn (errorBundlePretty err) >> pPrint s
