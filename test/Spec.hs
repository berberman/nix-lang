{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Function ((&))
import qualified ExactPrint
import Nix.Lang.ExactPrint
import Nix.Lang.QQ
import Nix.Lang.Types
import Nix.Lang.Utils
import Nix.Lang.Zipper
import Nix.Lang.Zipper.Edit
import Nix.Lang.Zipper.ExactPrint
import qualified Parser
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

joinZipperCloseResult :: Either ZipperError (CloseResult a) -> CloseResult a
joinZipperCloseResult = either (Left . CloseStructuralError) id

tests :: TestTree
tests =
  testGroup
    "nix-lang"
    [ ExactPrint.tests,
      Parser.tests,
      testGroup
        "zipper"
        [ testCase "A simple example of zipper API" zipperExample,
          testCase "closeExpr repairs root focus edits" zipperRootFocusRepair,
          testCase "closeExpr preserves unrelated let binding layout" zipperPreservesUnrelatedLetLayout,
          testCase "closeExpr repairs shape-changing replacement locally" zipperShapeChangingReplacement,
          testCase "closeExpr repairs list element replacement" zipperListElementReplacement
        ]
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
  let closed =
        rootFocus expr
          & downChild LetB
          >>= downChild AppA
          >>= downItem SSet 0
          >>= downChild BindingV
          >>= downChild ExprLit
          >>= pure . replaceIntLiteral 233
  case joinZipperCloseResult closed of
    Right x -> renderExactText (unLoc x) @?= "let\n  f = {a, b}: a + b;\nin\n  f { a = 233; b = 2; }"
    Left err -> fail $ show err

zipperPreservesUnrelatedLetLayout :: Assertion
zipperPreservesUnrelatedLetLayout = do
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
  let closed =
        rootFocus expr
          & downChild LetB
          >>= downChild AppA
          >>= downItem SSet 0
          >>= downChild BindingV
          >>= downChild ExprLit
          >>= pure . replaceIntLiteral 233
  case joinZipperCloseResult closed of
    Right x ->
      renderExactText (unLoc x)
        @?= "let\n  f = {a, b}: a + b;\n  g = [\n    1\n    2\n  ];\nin\n  f { a = 233; b = 2; }"
    Left err -> fail $ show err

zipperRootFocusRepair :: Assertion
zipperRootFocusRepair = do
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
  let closed = replaceExpr replacement (rootFocus expr)
  case closed of
    Right x ->
      renderExactText (unLoc x)
        @?= "{\n  a = 2;\n  b = [\n    3\n    4\n  ];\n}"
    Left err -> fail $ show err

zipperShapeChangingReplacement :: Assertion
zipperShapeChangingReplacement = do
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
  let closed =
        rootFocus expr
          & downChild LetB
          >>= downItem SList 0
          >>= pure . replaceExpr replacement
  case joinZipperCloseResult closed of
    Right x ->
      renderExactText (unLoc x)
        @?= "let\n  value = 1;\nin\n  [\n    value 20\n    2\n            ]"
    Left err -> fail $ show err

zipperListElementReplacement :: Assertion
zipperListElementReplacement = do
  let expr :: LExpr =
        [nixQQ|
          [
            1
            2
            3
          ]
        |]
  let closed =
        rootFocus expr
          & downItem SList 1
          >>= downChild ExprLit
          >>= pure . replaceIntLiteral 200
  case joinZipperCloseResult closed of
    Right x ->
      renderExactText (unLoc x)
        @?= "[\n  1\n  200\n  3\n]"
    Left err -> fail $ show err
