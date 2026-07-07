{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Lang.Edit
import Nix.Lang.ExactPrint (renderExactText)
import Nix.Lang.QQ (nixQQ)
import Nix.Lang.QQ.Parsed (nixParsedQQ)
import Nix.Lang.RFCPrint (formatExpr)
import qualified Nix.Lang.Types.Ps as Ps
import Nix.Lang.Types.Syn
import Nix.Lang.Utils (unLoc)
import Test.Tasty
import Test.Tasty.HUnit

pkg :: Text -> Expr
pkg name = mkSelectAttrs (mkVar "pkgs") [name]

devShellExpr :: [Text] -> Expr
devShellExpr packageNames =
  mkApp
    (mkSelectAttrs (mkVar "pkgs") ["mkShell"])
    ( mkStaticSet
        [ ("packages", mkList (pkg <$> packageNames))
        ]
    )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "nix-lang-qq"
    [ testCase "builds Syn expressions" $ do
        formatExpr ([nixQQ| let x = 1; in x |] :: Expr)
          @?= "let\n  x = 1;\nin\nx",
      testCase "splices antiquoted Syn expressions" $ do
        let value = mkInt 233
        formatExpr ([nixQQ| { x = %%(value); } |] :: Expr)
          @?= "{\n  x = 233;\n}",
      testCase "converts antiquoted integers automatically" $ do
        let value = (233 :: Integer)
        formatExpr ([nixQQ| { x = %%(value); } |] :: Expr)
          @?= "{\n  x = 233;\n}",
      testCase "converts antiquoted text automatically" $ do
        let value = ("hello" :: Text)
        formatExpr ([nixQQ| %%(value) |] :: Expr)
          @?= "\"hello\"",
      testCase "converts antiquoted string automatically" $ do
        let value = ("hello" :: String)
        formatExpr ([nixQQ| %%(value) |] :: Expr)
          @?= "\"hello\"",
      testCase "converts antiquoted bool automatically" $ do
        let value = True
        formatExpr ([nixQQ| %%(value) |] :: Expr)
          @?= "true",
      testCase "supports multiple antiquotes" $ do
        formatExpr ([nixQQ| { a = %%(mkInt 1); b = %%(mkInt 2); } |] :: Expr)
          @?= "{\n  a = 1;\n  b = 2;\n}",
      testCase "keeps non-antiquote percent text untouched" $ do
        formatExpr ([nixQQ| "% %%" |] :: Expr)
          @?= "\"% %%\"",
      testCase "supports nested antiquote expressions" $ do
        formatExpr ([nixQQ| { value = %%(mkApp (mkVar "f") (mkInt 1)); } |] :: Expr)
          @?= "{\n  value = f 1;\n}",
      testCase "captures quoted closing parens inside antiquote payload" $ do
        formatExpr ([nixQQ| %%(mkText "not ) the end") |] :: Expr)
          @?= "\"not ) the end\"",
      testCase "captures escaped quotes inside antiquote payload" $ do
        formatExpr ([nixQQ| %%(mkText "a\"b") |] :: Expr)
          @?= "\"a\\\"b\"",
      testCase "antiquote works inside string interpolation" $ do
        formatExpr ([nixQQ| "a${%%(mkVar "b")}c" |] :: Expr)
          @?= "\"a${b}c\"",
      testCase "supports devShell example" $ do
        formatExpr
          [nixQQ|
            let pkgs = import <nixpkgs> {}; in
              %%(devShellExpr ["ghc", "cabal-install"])
          |]
          @?= T.intercalate
            "\n"
            [ "let",
              "  pkgs = import <nixpkgs> { };",
              "in",
              "pkgs.mkShell {",
              "  packages = [",
              "    pkgs.ghc",
              "    pkgs.cabal-install",
              "  ];",
              "}"
            ],
      testCase "rejects placeholder collision" $ do
        formatExpr
          [nixQQ|
              let __nix_lang_qq_antiquote_0 = 1; in
               [ __nix_lang_qq_antiquote_0 %%(mkInt 2) ]
             |]
          @?= "let\n  __nix_lang_qq_antiquote_0 = 1;\nin\n[\n  __nix_lang_qq_antiquote_0\n  2\n]",
      testCase "builds parsed located expressions" $ do
        renderExactText (unLoc ([nixParsedQQ| let x = 1; in x |] :: Ps.LExpr))
          @?= "let x = 1; in x",
      testCase "parsed quasiquoter works with editExpr" $ do
        let expr =
              [nixParsedQQ|
                  {
                    a = 1;
                    b = 2;
                  }
                |] ::
                Ps.LExpr
        case editExpr (bindingByKey "b" // bindingValue) (setIntLiteral 20) expr of
          Right x -> renderExactText (unLoc x) @?= "{\n  a = 1;\n  b = 20;\n}"
          Left err -> fail $ show err
    ]
