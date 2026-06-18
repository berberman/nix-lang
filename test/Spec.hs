{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Function ((&))
import qualified Data.Text.IO as T
import qualified ExactPrint
import Nix.Lang.ExactPrint
import Nix.Lang.QQ
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Nix.Lang.Zipper
import qualified Parser
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "nix-lang"
    [ ExactPrint.tests,
      Parser.tests,
      testGroup
        "zipper"
        [testCase "A simple example of zipper API" zipperExample]
    ]

zipperExample :: Assertion
zipperExample = do
  let expr :: LExpr =
        [nixQQ|
          let
            f = {a, b}: a + b;
          in
            f { a = 1; b = 2; }
        |]
  let expr' =
        rootFocus expr
          & downChild LetB
          >>= downChild AppA
          >>= downItem SSet 0
          >>= downChild BindingV
          >>= pure
            . modifyFocus
              ( \case
                  (L l (NixLit ann (L l' _))) -> L l $ NixLit ann $ L l' $ NixInteger NoExtF 233
                  _ -> error "Unexpected expression"
              )
          >>= closeExpr
  case expr' of
    Right x -> renderExactPrint (unLoc x) @?= "let\n  f = {a, b}: a + b;\nin\n  f { a = 233; b = 2; }"
    Left err -> fail $ show err
