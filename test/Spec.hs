{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import qualified ExactPrint
import Nix.Lang.Edit
import Nix.Lang.ExactPrint
import Nix.Lang.QQ
import Nix.Lang.Types
import Nix.Lang.Utils
import qualified Parser
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "nix-lang"
    [ Parser.tests,
      ExactPrint.tests,
      testGroup
        "edit"
        [ testCase "editExpr supports nested selector updates" editExprNestedSelectorUpdate,
          testCase "editExpr repairs root replacements" editExprRootReplacement,
          testCase "editExpr preserves unrelated let binding layout" editExprPreservesUnrelatedLetLayout,
          testCase "editExpr repairs shape-changing replacement locally" editExprShapeChangingReplacement,
          testCase "editExpr repairs list element replacement" editExprListElementReplacement,
          testCase "editExpr supports binding lookup by key" editExprBindingLookupByKey,
          testCase "editExpr supports binding lookup by path" editExprBindingLookupByPath,
          testCase "editExpr supports let binding insertion" editExprLetBindingInsertion,
          testCase "editExpr supports set binding insertion" editExprSetBindingInsertion,
          testCase "editExpr supports inherit binding insertion" editExprInheritBindingInsertion,
          testCase "editBinding supports inherit scope updates" editBindingInheritScopeUpdate,
          testCase "editBinding supports inherit key updates" editBindingInheritKeyUpdate,
          testCase "editBinding supports inherit key insertion" editBindingInheritKeyInsertion
        ]
    ]

editExprNestedSelectorUpdate :: Assertion
editExprNestedSelectorUpdate = do
  let expr :: LExpr =
        [nixQQ|
          let
            f = {a, b}: a + b;
          in
            f { a = 1; b = 2; }
        |]
  case editExpr (letBody // appArgument // bindingAt 0 // bindingValue) (setIntLiteral 233) expr of
    Right x -> renderExactText (unLoc x) @?= "let\n  f = {a, b}: a + b;\nin\n  f { a = 233; b = 2; }"
    Left err -> fail $ show err

editExprPreservesUnrelatedLetLayout :: Assertion
editExprPreservesUnrelatedLetLayout = do
  let expr :: LExpr =
        [nixQQ|
          let
            f = {a, b}: a + b;
            g = [
              1
              2
            ];
          in
            f { a = 1; b = 2; }
        |]
  case editExpr (letBody // appArgument // bindingAt 0 // bindingValue) (setIntLiteral 233) expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "let\n  f = {a, b}: a + b;\n  g = [\n    1\n    2\n  ];\nin\n  f { a = 233; b = 2; }"
    Left err -> fail $ show err

editExprRootReplacement :: Assertion
editExprRootReplacement = do
  let expr :: LExpr =
        [nixQQ|
          {
            a = 1;
            b = [
              2
              3
            ];
          }
        |]
      replacement :: LExpr =
        [nixQQ|
          {
            a = 2;
            b = [
              3
              4
            ];
          }
        |]
  case editExpr root (replace replacement) expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "{\n  a = 2;\n  b = [\n    3\n    4\n  ];\n}"
    Left err -> fail $ show err

editExprShapeChangingReplacement :: Assertion
editExprShapeChangingReplacement = do
  let expr :: LExpr =
        [nixQQ|
          let
            value = 1;
          in
            [ value 2 ]
        |]
      replacement :: LExpr =
        [nixQQ|
          value 20
        |]
  case editExpr (letBody // listElement 0) (replace replacement) expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "let\n  value = 1;\nin\n  [\n    value 20\n    2\n            ]"
    Left err -> fail $ show err

editExprListElementReplacement :: Assertion
editExprListElementReplacement = do
  let expr :: LExpr =
        [nixQQ|
          [
            1
            2
            3
          ]
        |]
  case editExpr (listElement 1) (setIntLiteral 200) expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "[\n  1\n  200\n  3\n]"
    Left err -> fail $ show err

editExprBindingLookupByKey :: Assertion
editExprBindingLookupByKey = do
  let expr :: LExpr =
        [nixQQ|
          {
            a = 1;
            b = 2;
          }
        |]
  case editExpr (bindingByKey "b" // bindingValue) (setIntLiteral 20) expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "{\n  a = 1;\n  b = 20;\n}"
    Left err -> fail $ show err

editExprBindingLookupByPath :: Assertion
editExprBindingLookupByPath = do
  let expr :: LExpr =
        [nixQQ|
          {
            services.nginx.enable = false;
          }
        |]
  case editExpr (bindingByPath ["services", "nginx", "enable"] // bindingValue) (replaceExprText "true") expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "{\n  services.nginx.enable = true;\n}"
    Left err -> fail $ show err

editExprLetBindingInsertion :: Assertion
editExprLetBindingInsertion = do
  let expr :: LExpr =
        [nixQQ|
          let
            a = 1;
          in
            a
        |]
  case editExpr root (insertBindingText (InsertBindingAt 1) "b = 2;") expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "let\n  a = 1;\n  b = 2;\nin\n  a"
    Left err -> fail $ show err

editExprSetBindingInsertion :: Assertion
editExprSetBindingInsertion = do
  let expr :: LExpr =
        [nixQQ|
          {
            a = 1;
            c = 3;
          }
        |]
  case editExpr root (insertBindingText (InsertBindingAt 1) "b = 2;") expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "{\n  a = 1;\n  b = 2;\n  c = 3;\n}"
    Left err -> fail $ show err

editExprInheritBindingInsertion :: Assertion
editExprInheritBindingInsertion = do
  let expr :: LExpr =
        [nixQQ|
          {
            a = 1;
          }
        |]
  case editExpr root (insertBindingText (InsertBindingAt 0) "inherit (pkgs) lib;") expr of
    Right x ->
      renderExactText (unLoc x)
        @?= "{\n  inherit (pkgs) lib;\n  a = 1;\n}"
    Left err -> fail $ show err

editBindingInheritScopeUpdate :: Assertion
editBindingInheritScopeUpdate = do
  let expr :: LExpr =
        [nixQQ|
          {
            inherit (pkgs) lib;
          }
        |]
  case editExpr (bindingByKey "lib" // inheritScope) (replaceExprText "scope") expr of
    Right x -> renderExactText (unLoc x) @?= "{\n  inherit (scope) lib;\n}"
    Left err -> fail $ show err

editBindingInheritKeyUpdate :: Assertion
editBindingInheritKeyUpdate = do
  let expr :: LExpr =
        [nixQQ|
          {
            inherit (pkgs) old other;
          }
        |]
  case editExpr (bindingByKey "old" // inheritKey "old") (replaceAttrKeyText "new") expr of
    Right x -> renderExactText (unLoc x) @?= "{\n  inherit (pkgs) new other;\n}"
    Left err -> fail $ show err

editBindingInheritKeyInsertion :: Assertion
editBindingInheritKeyInsertion = do
  let expr :: LExpr =
        [nixQQ|
          {
            inherit (pkgs) a c;
          }
        |]
  case editExpr (bindingByKey "a") (insertInheritKeyTextAt 1 "b") expr of
    Right x -> renderExactText (unLoc x) @?= "{\n  inherit (pkgs) a b c;\n}"
    Left err -> fail $ show err
