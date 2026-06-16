module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Nix.Lang.Parser
import Nix.Lang.Types
import Nix.Lang.Utils (isSubspanOf, mkSrcSpan)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "nix-lang"
    [ testGroup
        "parser"
        [ testCase "sample fixture parses" sampleFixtureParses,
          testGroup
            "literals and atoms"
            [ testCase "integer literal" integerLiteralParses,
              testCase "float literal" floatLiteralParses,
              testCase "boolean literal" booleanLiteralParses,
              testCase "null literal" nullLiteralParses,
              testCase "uri literal" uriLiteralParses,
              testCase "variable" variableParses,
              testCase "parenthesized expression" parensParse,
              testCase "environment path" envPathParses,
              testCase "literal path" literalPathParses,
              testCase "interpolated path" interpolatedPathParses,
              testCase "underscore-starting path" underscorePathParses,
              testCase "alpha-starting path" alphaPathParses,
              testCase "rec-starting path" recPathParses
            ],
          testGroup
            "strings"
            [ testCase "double quoted string with interpolation" doubleQuotedStringParses,
              testCase "indented string with interpolation" indentedStringParses
            ],
          testGroup
            "structures and bindings"
            [ testCase "empty set" emptySetParses,
              testCase "recursive set" recursiveSetParses,
              testCase "inherit binding" inheritBindingParses,
              testCase "dynamic attribute binding" dynamicAttrBindingParses,
              testCase "list expression" listParses
            ],
          testGroup
            "functions and control"
            [ testCase "assert expression parses to NixAssert" assertParsesToAssert,
              testCase "with expression" withParses,
              testCase "if expression" ifParses,
              testCase "let expression" letParses,
              testCase "lambda variable pattern" lambdaVarPatParses,
              testCase "lambda set pattern" lambdaSetPatParses,
              testCase "lambda trailing as-pattern" lambdaTrailingAsParses
            ],
          testGroup
            "selection and operators"
            [ testCase "selection with default" selectOrParses,
              testCase "has-attr operator" hasAttrParses,
              testCase "application precedence" applicationParses,
              testCase "binary operator precedence" operatorPrecedenceParses,
              testCase "unary operators" unaryOperatorsParse
            ],
          testGroup
            "unsupported syntax"
            [ testCase "legacy let is rejected" legacyLetRejected,
              testCase "leading-decimal float is rejected" leadingDecimalRejected,
              testCase "multiple has-attr is rejected" multiHasAttrRejected,
              testCase "operator without whitespace is rejected" operatorWithoutWhitespaceRejected
            ]
        ],
      testGroup
        "parser state and spans"
        [ testCase "line comments are collected" lineCommentsCollected,
          testCase "block comments are collected" blockCommentsCollected,
          testCase "assert annotations include keyword and semicolon" assertAnnotationsCollected,
          testCase "isSubspanOf checks end columns" isSubspanOfChecksEndColumns
        ]
    ]

sampleFixtureParses :: Assertion
sampleFixtureParses = do
  src <- T.readFile "test/sample.nix"
  case runNixParser nixFile "test/sample.nix" src of
    (Right _, _) -> pure ()
    (Left err, _) -> assertFailure $ errorBundlePretty err

parseExprOrFail :: T.Text -> IO (NixExpr Ps)
parseExprOrFail src =
  case runNixParser (nixExpr <* eof) "<expr>" src of
    (Right expr, _) -> pure expr
    (Left err, _) -> assertFailure (errorBundlePretty err) >> error "unreachable"

parseExprWithStateOrFail :: T.Text -> IO (NixExpr Ps, PState)
parseExprWithStateOrFail src =
  case runNixParser (nixExpr <* eof) "<expr>" src of
    (Right expr, st) -> pure (expr, st)
    (Left err, _) -> assertFailure (errorBundlePretty err) >> error "unreachable"

parseFileWithStateOrFail :: FilePath -> T.Text -> IO (NixExpr Ps, PState)
parseFileWithStateOrFail fp src =
  case runNixParser nixFile fp src of
    (Right expr, st) -> pure (expr, st)
    (Left err, _) -> assertFailure (errorBundlePretty err) >> error "unreachable"

parseExprFails :: T.Text -> Assertion
parseExprFails src =
  case runNixParser (nixExpr <* eof) "<expr>" src of
    (Left _, _) -> pure ()
    (Right expr, _) -> assertFailure $ "expected parse failure, got: " <> show expr

integerLiteralParses :: Assertion
integerLiteralParses = do
  expr <- parseExprOrFail "233"
  case expr of
    NixLit _ (L _ (NixInteger _ 233)) -> pure ()
    _ -> assertFailure $ "expected integer literal, got: " <> show expr

floatLiteralParses :: Assertion
floatLiteralParses = do
  expr <- parseExprOrFail "2.33"
  case expr of
    NixLit _ (L _ (NixFloat _ f)) | abs (f - 2.33) < 0.0001 -> pure ()
    _ -> assertFailure $ "expected float literal, got: " <> show expr

booleanLiteralParses :: Assertion
booleanLiteralParses = do
  t <- parseExprOrFail "true"
  f <- parseExprOrFail "false"
  case (t, f) of
    (NixLit _ (L _ (NixBoolean _ True)), NixLit _ (L _ (NixBoolean _ False))) -> pure ()
    _ -> assertFailure "expected true/false boolean literals"

nullLiteralParses :: Assertion
nullLiteralParses = do
  expr <- parseExprOrFail "null"
  case expr of
    NixLit _ (L _ (NixNull _)) -> pure ()
    _ -> assertFailure $ "expected null literal, got: " <> show expr

uriLiteralParses :: Assertion
uriLiteralParses = do
  expr <- parseExprOrFail "https://www.example.com"
  case expr of
    NixLit _ (L _ (NixUri _ uri)) | uri == "https://www.example.com" -> pure ()
    _ -> assertFailure $ "expected uri literal, got: " <> show expr

variableParses :: Assertion
variableParses = do
  expr <- parseExprOrFail "hello_world"
  case expr of
    NixVar _ (L _ name) | name == "hello_world" -> pure ()
    _ -> assertFailure $ "expected variable, got: " <> show expr

parensParse :: Assertion
parensParse = do
  expr <- parseExprOrFail "(1)"
  case expr of
    NixPar _ (L _ (NixLit _ (L _ (NixInteger _ 1)))) -> pure ()
    _ -> assertFailure $ "expected parenthesized integer, got: " <> show expr

envPathParses :: Assertion
envPathParses = do
  expr <- parseExprOrFail "<nixpkgs/nixos>"
  case expr of
    NixEnvPath _ (L _ path) | path == "nixpkgs/nixos" -> pure ()
    _ -> assertFailure $ "expected environment path, got: " <> show expr

literalPathParses :: Assertion
literalPathParses = do
  expr <- parseExprOrFail "./nix"
  case expr of
    NixPath _ (L _ (NixLiteralPath _ path)) | path == "./nix" -> pure ()
    _ -> assertFailure $ "expected literal path, got: " <> show expr

interpolatedPathParses :: Assertion
interpolatedPathParses = do
  expr <- parseExprOrFail "./${a}-${b}/c/d${e}"
  case expr of
    NixPath _ (L _ (NixInterpolPath (SourceText src) parts)) -> do
      src @?= "./${a}-${b}/c/d${e}"
      length parts @?= 6
    _ -> assertFailure $ "expected interpolated path, got: " <> show expr

underscorePathParses :: Assertion
underscorePathParses = do
  expr <- parseExprOrFail "_/foo"
  case expr of
    NixPath _ (L _ (NixLiteralPath _ path)) | path == "_/foo" -> pure ()
    _ -> assertFailure $ "expected underscore-starting path, got: " <> show expr

alphaPathParses :: Assertion
alphaPathParses = do
  expr <- parseExprOrFail "abc/foo"
  case expr of
    NixPath _ (L _ (NixLiteralPath _ path)) | path == "abc/foo" -> pure ()
    _ -> assertFailure $ "expected alpha-starting path, got: " <> show expr

recPathParses :: Assertion
recPathParses = do
  expr <- parseExprOrFail "rec/foo"
  case expr of
    NixPath _ (L _ (NixLiteralPath _ path)) | path == "rec/foo" -> pure ()
    _ -> assertFailure $ "expected rec-starting path, got: " <> show expr

doubleQuotedStringParses :: Assertion
doubleQuotedStringParses = do
  expr <- parseExprOrFail "\"a${b}c\""
  case expr of
    NixString _ (L _ (NixDoubleQuotesString (SourceText src) parts)) -> do
      src @?= "a${b}c"
      length parts @?= 3
    _ -> assertFailure $ "expected double quoted string, got: " <> show expr

indentedStringParses :: Assertion
indentedStringParses = do
  expr <- parseExprOrFail "''a${b}c''"
  case expr of
    NixString _ (L _ (NixDoubleSingleQuotesString (SourceText src) parts)) -> do
      src @?= "a${b}c"
      length parts @?= 3
    _ -> assertFailure $ "expected indented string, got: " <> show expr

emptySetParses :: Assertion
emptySetParses = do
  expr <- parseExprOrFail "{ }"
  case expr of
    NixSet _ NixSetNonRecursive (L _ []) -> pure ()
    _ -> assertFailure $ "expected empty set, got: " <> show expr

recursiveSetParses :: Assertion
recursiveSetParses = do
  expr <- parseExprOrFail "rec { k = 233; }"
  case expr of
    NixSet _ NixSetRecursive (L _ [L _ (NixNormalBinding _ _ (L _ (NixLit _ (L _ (NixInteger _ 233)))))]) -> pure ()
    _ -> assertFailure $ "expected recursive set, got: " <> show expr

inheritBindingParses :: Assertion
inheritBindingParses = do
  expr <- parseExprOrFail "{ inherit (s) m g; }"
  case expr of
    NixSet _ _ (L _ [L _ (NixInheritBinding _ (Just (L _ (NixPar _ _))) keys)]) -> length keys @?= 2
    _ -> assertFailure $ "expected inherit binding set, got: " <> show expr

dynamicAttrBindingParses :: Assertion
dynamicAttrBindingParses = do
  expr <- parseExprOrFail "{ \"${dynamic}\".${attr} = g; }"
  case expr of
    NixSet _ _ (L _ [L _ (NixNormalBinding _ (L _ (NixAttrPath keys)) (L _ (NixVar _ (L _ "g"))))]) ->
      length keys @?= 2
    _ -> assertFailure $ "expected dynamic attr binding, got: " <> show expr

listParses :: Assertion
listParses = do
  expr <- parseExprOrFail "[1 2 3]"
  case expr of
    NixList _ xs -> length xs @?= 3
    _ -> assertFailure $ "expected list, got: " <> show expr

assertParsesToAssert :: Assertion
assertParsesToAssert =
  case runNixParser (nixExpr <* eof) "<assert>" (T.pack "assert a == 1; 2") of
    (Right (NixAssert _ _ _), _) -> pure ()
    (Right expr, _) -> assertFailure $ "expected NixAssert, got: " <> show expr
    (Left err, _) -> assertFailure $ errorBundlePretty err

withParses :: Assertion
withParses = do
  expr <- parseExprOrFail "with l; 1"
  case expr of
    NixWith _ (L _ (NixVar _ (L _ "l"))) (L _ (NixLit _ (L _ (NixInteger _ 1)))) -> pure ()
    _ -> assertFailure $ "expected with expression, got: " <> show expr

ifParses :: Assertion
ifParses = do
  expr <- parseExprOrFail "if a then b else c"
  case expr of
    NixIf _ (L _ (NixVar _ (L _ "a"))) (L _ (NixVar _ (L _ "b"))) (L _ (NixVar _ (L _ "c"))) -> pure ()
    _ -> assertFailure $ "expected if expression, got: " <> show expr

letParses :: Assertion
letParses = do
  expr <- parseExprOrFail "let x = 1; in x"
  case expr of
    NixLet _ (L _ [L _ (NixNormalBinding _ _ _)]) (L _ (NixVar _ (L _ "x"))) -> pure ()
    _ -> assertFailure $ "expected let expression, got: " <> show expr

lambdaVarPatParses :: Assertion
lambdaVarPatParses = do
  expr <- parseExprOrFail "x: x"
  case expr of
    NixLam _ (L _ (NixVarPat _ (L _ "x"))) (L _ (NixVar _ (L _ "x"))) -> pure ()
    _ -> assertFailure $ "expected lambda with var pattern, got: " <> show expr

lambdaSetPatParses :: Assertion
lambdaSetPatParses = do
  expr <- parseExprOrFail "{x ? 1, y ? {}, ...}: x"
  case expr of
    NixLam _ (L _ (NixSetPat _ NixSetPatIsEllipses Nothing bindings)) _ -> length bindings @?= 2
    _ -> assertFailure $ "expected lambda with set pattern, got: " <> show expr

lambdaTrailingAsParses :: Assertion
lambdaTrailingAsParses = do
  expr <- parseExprOrFail "{x}@a: x"
  case expr of
    NixLam _ (L _ (NixSetPat _ _ (Just (L _ NixSetPatAs {nspaLocation = NixSetPatAsTrailing, nspaVar = L _ "a"})) _)) _ -> pure ()
    _ -> assertFailure $ "expected lambda with trailing as-pattern, got: " <> show expr

selectOrParses :: Assertion
selectOrParses = do
  expr <- parseExprOrFail "a.b.c or d.e"
  case expr of
    NixSelect _ (L _ (NixVar _ (L _ "a"))) (L _ (NixAttrPath keys)) (Just (L _ (NixSelect _ _ _ Nothing))) ->
      length keys @?= 2
    _ -> assertFailure $ "expected selection with default, got: " <> show expr

hasAttrParses :: Assertion
hasAttrParses = do
  expr <- parseExprOrFail "a ? b"
  case expr of
    NixHasAttr _ (L _ (NixVar _ (L _ "a"))) (L _ (NixAttrPath keys)) -> length keys @?= 1
    _ -> assertFailure $ "expected has-attr expression, got: " <> show expr

applicationParses :: Assertion
applicationParses = do
  expr <- parseExprOrFail "a b c"
  case expr of
    NixApp _ (L _ (NixApp _ (L _ (NixVar _ (L _ "a"))) (L _ (NixVar _ (L _ "b"))))) (L _ (NixVar _ (L _ "c"))) -> pure ()
    _ -> assertFailure $ "expected left-associative application, got: " <> show expr

operatorPrecedenceParses :: Assertion
operatorPrecedenceParses = do
  expr <- parseExprOrFail "a >= b || c < d && e == f"
  case expr of
    NixBinApp _ OpOr (L _ (NixBinApp _ OpGE _ _)) (L _ (NixBinApp _ OpAnd (L _ (NixBinApp _ OpLT _ _)) (L _ (NixBinApp _ OpEq _ _)))) -> pure ()
    _ -> assertFailure $ "expected precedence tree, got: " <> show expr

unaryOperatorsParse :: Assertion
unaryOperatorsParse = do
  expr <- parseExprOrFail "!true || -5 == -5"
  case expr of
    NixBinApp _ OpOr (L _ (NixNotApp _ _)) (L _ (NixBinApp _ OpEq (L _ (NixNegApp _ _)) (L _ (NixNegApp _ _)))) -> pure ()
    _ -> assertFailure $ "expected unary operators, got: " <> show expr

legacyLetRejected :: Assertion
legacyLetRejected = parseExprFails "let { x = 233; body = x; }"

leadingDecimalRejected :: Assertion
leadingDecimalRejected = parseExprFails ".233"

multiHasAttrRejected :: Assertion
multiHasAttrRejected = parseExprFails "a ? b ? c ? d"

operatorWithoutWhitespaceRejected :: Assertion
operatorWithoutWhitespaceRejected = parseExprFails "1+-1"

lineCommentsCollected :: Assertion
lineCommentsCollected = do
  (_, st) <- parseFileWithStateOrFail "<comments>" "{\n  # hello\n  x = 1;\n}"
  let comments = concatMap snd (psComments st)
  assertBool "expected collected line comment" $ any isHelloLine comments
  where
    isHelloLine (L _ (LineComment txt)) = txt == " hello"
    isHelloLine _ = False

blockCommentsCollected :: Assertion
blockCommentsCollected = do
  (_, st) <- parseFileWithStateOrFail "<comments>" "[ a /* hello */ ]"
  let comments = concatMap snd (psComments st)
  assertBool "expected collected block comment" $ any isHelloBlock comments
  where
    isHelloBlock (L _ (BlockComment txt)) = txt == " hello "
    isHelloBlock _ = False

assertAnnotationsCollected :: Assertion
assertAnnotationsCollected = do
  (_, st) <- parseExprWithStateOrFail "assert a == 1; 2"
  let anns = psAnnotation st
  assertBool "expected assert keyword annotation" $ any hasAssert anns
  assertBool "expected semicolon annotation" $ any hasSemicolon anns
  where
    hasAssert (AddAnn _ AnnAssert _) = True
    hasAssert _ = False
    hasSemicolon (AddAnn _ AnnSemicolon _) = True
    hasSemicolon _ = False

isSubspanOfChecksEndColumns :: Assertion
isSubspanOfChecksEndColumns = do
  let parent = mkSrcSpan "<test>" (1, 1) (1, 5)
      inside = mkSrcSpan "<test>" (1, 2) (1, 5)
      outside = mkSrcSpan "<test>" (1, 2) (1, 6)
  assertBool "subspan ending at parent column should be inside" $ inside `isSubspanOf` parent
  assertBool "subspan ending after parent column should be outside" $ not (outside `isSubspanOf` parent)
