module Main (main) where

import Control.Monad.State.Strict (forM_)
import Data.List (dropWhileEnd)
import qualified Data.Text.IO as T
import Nix.Lang.Parser
import Nix.Lang.Pretty
import Prettyprinter.Util (putDocW)
import System.Directory.Tree
import System.FilePath (takeExtension)
import System.Process
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)
import Data.Functor (($>))

main :: IO ()
main = nixpkgs

nixpkgs :: IO ()
nixpkgs = do
  src <- dropWhileEnd (== '\n') <$> readProcess "nix-instantiate" ["--eval", "-E", "<nixpkgs>"] ""
  print src
  tree <- readDirectoryWithL (\x -> if takeExtension x == ".nix" then fmap (Just . (x,)) <$> T.readFile $ x else pure Nothing) (src <> "/nixos/")
  forM_ (dirTree tree) $ \case
    Just (name, content) -> case runNixParser nixFile name content of
      (Right _, _) -> putStrLn $ name <> " ok"
      (Left err, _) -> putStrLn name >> putStrLn (errorBundlePretty err)
    _ -> pure ()

test :: (Show a, PrettyNix a) => Parser a -> IO a
test parser =
  T.readFile "test.nix" >>= \src ->
    case runNixParser parser "test.nix" src of
      (Right x, s) -> pPrint x >> putDocW 80 (prettyNix x) >> pPrint s $> x
      (Left err, s) -> pPrint s >> error (errorBundlePretty err)

