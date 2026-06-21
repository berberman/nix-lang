module Utils where

import qualified Data.Text as T
import Nix.Lang.Parser
import Nix.Lang.Types
import Nix.Lang.Types.Parsed
import Test.Tasty.HUnit
import Text.Megaparsec (eof, errorBundlePretty)

parseExprOrFail :: T.Text -> IO (NixExpr Ps)
parseExprOrFail src =
  case runNixParser (nixExpr <* eof) "<expr>" src of
    (Right expr, _) -> pure expr
    (Left err, _) -> assertFailure (errorBundlePretty err)

parseFileOrFail :: FilePath -> T.Text -> IO (NixExpr Ps)
parseFileOrFail fp src =
  case runNixParser nixFile fp src of
    (Right expr, _) -> pure expr
    (Left err, _) -> assertFailure (errorBundlePretty err)

parseExprFails :: T.Text -> Assertion
parseExprFails src =
  case runNixParser (nixExpr <* eof) "<expr>" src of
    (Left _, _) -> pure ()
    (Right expr, _) -> assertFailure $ "expected parse failure, got: " <> show expr
