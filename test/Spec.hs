module Main (main) where

import Control.Monad.State.Strict (forM_, runState)
import qualified Data.Text.IO as T
import Nix.Lang.Parser
import Nix.Lang.Pretty
import Nix.Lang.Types
import Prettyprinter.Util (putDocW)
import System.Directory.Tree
import System.FilePath (takeExtension)
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  T.readFile "test.nix" >>= \src ->
    let m = runParserT nixFile "." src
     in case runState m $ PState [] [] [] (1, 1) of
          (Right x, s) -> pPrint x >> pPrint s -- >> putDocW 30 (prettyNixExpr x)
          (Left err, s) -> putStrLn (errorBundlePretty err) >> pPrint s

nixpkgs :: IO ()
nixpkgs = do
  tree <- readDirectoryWithL (\x -> if takeExtension x == ".nix" then fmap (Just . (x,)) <$> T.readFile $ x else pure Nothing) "nixpkgs/pkgs"
  forM_ (dirTree tree) $ \case
    Just (name, content) ->
      let m = runParserT nixFile name content
       in case runState m $ PState [] [] [] (1, 1) of
            (Right _, _) -> putStrLn $ name <> " ok"
            (Left err, _) -> putStrLn name >> putStrLn (errorBundlePretty err)
    _ -> pure ()

test :: (Show a) => Parser a -> IO ()
test parser =
  T.readFile "test.nix" >>= \src ->
    let m = runParserT parser "." src
     in case runState m $ PState [] [] [] (1, 1) of
          (Right x, s) -> pPrint x >> pPrint s
          (Left err, s) -> putStrLn (errorBundlePretty err) >> pPrint s
