module Parser where

import Control.Monad (filterM)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Nix.Lang.Annotation
import Nix.Lang.Parser
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Types.Ps
import Nix.Lang.Utils
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (makeRelative, takeExtension, (</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (eof, errorBundlePretty)
import Utils

tests :: TestTree
tests =
  testGroup
    "parser"
    [ testCase "sample fixture parses" sampleFixtureParses,
      nixpkgsParseTest,
      referenceParserFixtureTests,
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
          testCase "inherit binding carries typed annotation" inheritBindingCarriesTypedAnn,
          testCase "normal binding carries typed annotation" normalBindingCarriesTypedAnn,
          testCase "dynamic attribute binding" dynamicAttrBindingParses,
          testCase "list expression" listParses
        ],
      testGroup
        "functions and control"
        [ testCase "assert expression parses to NixAssert" assertParsesToAssert,
          testCase "assert carries typed annotation" assertCarriesTypedAnn,
          testCase "assert carries common absolute position" assertCarriesCommonAbsPosition,
          testCase "with expression" withParses,
          testCase "if expression" ifParses,
          testCase "let expression" letParses,
          testCase "lambda variable pattern" lambdaVarPatParses,
          testCase "lambda variable pattern carries typed annotation" lambdaVarPatCarriesTypedAnn,
          testCase "lambda set pattern" lambdaSetPatParses,
          testCase "lambda set pattern carries typed annotation" lambdaSetPatCarriesTypedAnn,
          testCase "lambda set pattern leading as owns at token" lambdaLeadingAsCarriesTypedAnn,
          testCase "lambda trailing as-pattern" lambdaTrailingAsParses
        ],
      testGroup
        "selection and operators"
        [ testCase "selection with default" selectOrParses,
          testCase "has-attr operator" hasAttrParses,
          testCase "has-attr carries typed annotation" hasAttrCarriesTypedAnn,
          testCase "application precedence" applicationParses,
          testCase "binary operator carries common absolute position" binaryOperatorCarriesCommonAbsPosition,
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
    ]

sampleFixtureParses :: Assertion
sampleFixtureParses = do
  src <- T.readFile "test/sample.nix"
  case runNixParser nixFile "test/sample.nix" src of
    (Right _, _) -> pure ()
    (Left err, _) -> assertFailure $ errorBundlePretty err

nixpkgsParseTest :: TestTree
nixpkgsParseTest =
  testCase "nixpkgs tree parses (if found)" $ do
    mRoot <- findNixpkgsRoot
    case mRoot of
      Nothing -> putStrLn "nixpkgs not found"
      Just root -> do
        exists <- doesDirectoryExist root
        if exists
          then assertNixpkgsCorpusParses root
          else assertFailure $ "<nixpkgs> path does not exist: " <> root

findNixpkgsRoot :: IO (Maybe FilePath)
findNixpkgsRoot = do
  (exitCode, stdout, _) <- readProcessWithExitCode "nix-instantiate" ["--find-file", "nixpkgs"] ""
  pure $ case exitCode of
    ExitSuccess ->
      case lines stdout of
        path : _ | not (null path) -> Just path
        _ -> Nothing
    _ -> Nothing

assertNixpkgsCorpusParses :: FilePath -> Assertion
assertNixpkgsCorpusParses root = do
  nixFiles <- listNixFiles root
  failures <- go [] nixFiles
  case failures of
    [] -> pure ()
    _ ->
      assertFailure . unlines $
        ["failed to parse " <> show (length failures) <> " nixpkgs files out of " <> show (length nixFiles)]
          <> fmap renderFailure (take 20 failures)
  where
    go !failures [] = pure (reverse failures)
    go !failures (fp : rest) = do
      src <- T.readFile fp
      case runNixParser nixFile fp src of
        (Right _, _) -> go failures rest
        (Left err, _) ->
          go ((makeRelative root fp, errorBundlePretty err) : failures) rest

    renderFailure (fp, err) = "FAIL " <> fp <> "\n" <> err

listNixFiles :: FilePath -> IO [FilePath]
listNixFiles root = do
  paths <- walk root
  pure $ List.sort [path | path <- paths, takeExtension path == ".nix"]
  where
    walk dir = do
      entries <- listDirectory dir
      let paths = fmap (dir </>) entries
      files <- filterM doesFileExist paths
      dirs <- filterM doesDirectoryExist paths
      nested <- go [] dirs
      pure (files <> nested)

    go acc [] = pure acc
    go acc (dir : rest) = do
      nested <- walk dir
      let acc' = nested <> acc
      acc' `seq` go acc' rest

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
    NixPath _ (L _ (NixInterpolPath NoExtF parts)) -> do
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

inheritBindingCarriesTypedAnn :: Assertion
inheritBindingCarriesTypedAnn = do
  expr <- parseExprOrFail "{ inherit (s) m g; }"
  case expr of
    NixSet _ _ (L _ [L _ (NixInheritBinding ann _ _)]) -> do
      fmap srcSpanStartColumn (annTokenSrcSpan (aibInherit ann)) @?= Just 3
      fmap srcSpanStartColumn (annTokenSrcSpan (aibSemicolon ann)) @?= Just 18
    _ -> assertFailure $ "expected inherit binding set, got: " <> show expr

dynamicAttrBindingParses :: Assertion
dynamicAttrBindingParses = do
  expr <- parseExprOrFail "{ \"${dynamic}\".${attr} = g; }"
  case expr of
    NixSet _ _ (L _ [L _ (NixNormalBinding _ (L _ (NixAttrPath _ [L _ (NixDynamicStringAttrKey NoExtF parts), L _ (NixDynamicInterpolAttrKey NoExtF (L _ (NixVar _ (L _ attr))))])) (L _ (NixVar _ (L _ "g"))))]) -> do
      length parts @?= 1
      attr @?= "attr"
    _ -> assertFailure $ "expected dynamic attr binding, got: " <> show expr

normalBindingCarriesTypedAnn :: Assertion
normalBindingCarriesTypedAnn = do
  expr <- parseExprOrFail "{ x = 1; }"
  case expr of
    NixSet _ _ (L _ [L _ (NixNormalBinding ann _ _)]) -> do
      fmap srcSpanStartColumn (annTokenSrcSpan (anbEqual ann)) @?= Just 5
      fmap srcSpanStartColumn (annTokenSrcSpan (anbSemicolon ann)) @?= Just 8
    _ -> assertFailure $ "expected normal binding set, got: " <> show expr

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

assertCarriesTypedAnn :: Assertion
assertCarriesTypedAnn =
  case runNixParser (nixExpr <* eof) "<assert>" (T.pack "assert a == 1; 2") of
    (Right (NixAssert ann _ _), _) -> do
      fmap srcSpanStartColumn (annTokenSrcSpan (aaAssert ann)) @?= Just 1
      fmap srcSpanStartColumn (annTokenSrcSpan (aaSemicolon ann)) @?= Just 14
      acComments (aaCommon ann) @?= emptyComments
    (Right expr, _) -> assertFailure $ "expected NixAssert, got: " <> show expr
    (Left err, _) -> assertFailure $ errorBundlePretty err

assertCarriesCommonAbsPosition :: Assertion
assertCarriesCommonAbsPosition =
  case runNixParser (nixExpr <* eof) "<assert>" (T.pack "assert a == 1; 2") of
    (Right (NixAssert ann _ _), _) -> do
      annPos ann @?= AnnSpan (mkSrcSpan "<assert>" (1, 1) (1, 17))
      annSrcSpan ann @?= Just (mkSrcSpan "<assert>" (1, 1) (1, 17))
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

lambdaVarPatCarriesTypedAnn :: Assertion
lambdaVarPatCarriesTypedAnn = do
  expr <- parseExprOrFail "x: x"
  case expr of
    NixLam ann (L _ (NixVarPat patAnn _)) _ -> do
      fmap srcSpanStartColumn (annTokenSrcSpan (alamColon ann)) @?= Just 2
      srcSpanStartColumn (avpId patAnn) @?= 1
    _ -> assertFailure $ "expected lambda with var pattern, got: " <> show expr

lambdaSetPatParses :: Assertion
lambdaSetPatParses = do
  expr <- parseExprOrFail "{x ? 1, y ? {}, ...}: x"
  case expr of
    NixLam _ (L _ (NixSetPat _ NixSetPatIsEllipses Nothing bindings)) _ -> length bindings @?= 2
    _ -> assertFailure $ "expected lambda with set pattern, got: " <> show expr

lambdaSetPatCarriesTypedAnn :: Assertion
lambdaSetPatCarriesTypedAnn = do
  expr <- parseExprOrFail "{x ? 1, y ? {}, ...}: x"
  case expr of
    NixLam lamAnn (L _ (NixSetPat _ _ _ bindings)) _ -> do
      fmap srcSpanStartColumn (annTokenSrcSpan (alamColon lamAnn)) @?= Just 21
      case bindings of
        L _ NixSetPatBinding {nspbAnn = AnnSetPatBinding {aspbQuestion = Just question}} : _ ->
          fmap srcSpanStartColumn (annTokenSrcSpan question) @?= Just 4
        _ -> assertFailure "expected first set-pattern binding to own question token"
    _ -> assertFailure $ "expected lambda with set pattern, got: " <> show expr

lambdaTrailingAsParses :: Assertion
lambdaTrailingAsParses = do
  expr <- parseExprOrFail "{x}@a: x"
  case expr of
    NixLam _ (L _ (NixSetPat _ _ (Just (L _ NixSetPatAs {nspaLocation = NixSetPatAsTrailing, nspaVar = L _ "a"})) _)) _ -> pure ()
    _ -> assertFailure $ "expected lambda with trailing as-pattern, got: " <> show expr

lambdaLeadingAsCarriesTypedAnn :: Assertion
lambdaLeadingAsCarriesTypedAnn = do
  expr <- parseExprOrFail "a@{x}: x"
  case expr of
    NixLam _ (L _ (NixSetPat _ _ (Just (L _ NixSetPatAs {nspaAnn = AnnSetPatAs {aspaAt = atTok}, nspaLocation = NixSetPatAsLeading, nspaVar = L _ "a"})) _)) _ ->
      fmap srcSpanStartColumn (annTokenSrcSpan atTok) @?= Just 2
    _ -> assertFailure $ "expected lambda with leading as-pattern token ownership, got: " <> show expr

selectOrParses :: Assertion
selectOrParses = do
  expr <- parseExprOrFail "a.b.c or d.e"
  case expr of
    NixSelect _ (L _ (NixVar _ (L _ "a"))) (L _ (NixAttrPath _ keys)) (Just (L _ (NixSelect _ _ _ Nothing))) ->
      length keys @?= 2
    _ -> assertFailure $ "expected selection with default, got: " <> show expr

hasAttrParses :: Assertion
hasAttrParses = do
  expr <- parseExprOrFail "a ? b"
  case expr of
    NixHasAttr _ (L _ (NixVar _ (L _ "a"))) (L _ (NixAttrPath _ keys)) -> length keys @?= 1
    _ -> assertFailure $ "expected has-attr expression, got: " <> show expr

hasAttrCarriesTypedAnn :: Assertion
hasAttrCarriesTypedAnn = do
  expr <- parseExprOrFail "a ? b"
  case expr of
    NixHasAttr ann _ _ -> fmap srcSpanStartColumn (annTokenSrcSpan (ahaQuestion ann)) @?= Just 3
    _ -> assertFailure $ "expected has-attr expression, got: " <> show expr

applicationParses :: Assertion
applicationParses = do
  expr <- parseExprOrFail "a b c"
  case expr of
    NixApp _ (L _ (NixApp _ (L _ (NixVar _ (L _ "a"))) (L _ (NixVar _ (L _ "b"))))) (L _ (NixVar _ (L _ "c"))) -> pure ()
    _ -> assertFailure $ "expected left-associative application, got: " <> show expr

binaryOperatorCarriesCommonAbsPosition :: Assertion
binaryOperatorCarriesCommonAbsPosition = do
  expr <- parseExprOrFail "a + b"
  case expr of
    NixBinApp ann OpAdd _ _ -> do
      annPos ann @?= AnnSpan (mkSrcSpan "<expr>" (1, 1) (1, 6))
      annSrcSpan ann @?= Just (mkSrcSpan "<expr>" (1, 1) (1, 6))
    _ -> assertFailure $ "expected binary application, got: " <> show expr

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

referenceParserFixtureTests :: TestTree
referenceParserFixtureTests =
  testGroup
    "reference parser fixtures"
    [ testGroup "valid parse" (fixtureParseCases <$> validFixtures),
      testGroup "invalid parse" (fixtureRejectCases <$> invalidFixtures),
      testGroup "known divergence" (fixtureAcceptedCases <$> acceptedInvalidFixtures)
    ]

fixtureParseCases :: FilePath -> TestTree
fixtureParseCases relPath =
  testCase relPath $ do
    src <- T.readFile fullPath
    case runNixParser nixFile fullPath src of
      (Right _, _) -> pure ()
      (Left err, _) -> assertFailure $ errorBundlePretty err
  where
    fullPath = fixtureRoot <> "/" <> relPath

fixtureRejectCases :: FilePath -> TestTree
fixtureRejectCases relPath =
  testCase relPath $ do
    src <- T.readFile fullPath
    parseFileFails fullPath src
  where
    fullPath = fixtureRoot <> "/" <> relPath

fixtureAcceptedCases :: FilePath -> TestTree
fixtureAcceptedCases relPath =
  testCase relPath $ do
    src <- T.readFile fullPath
    case runNixParser nixFile fullPath src of
      (Right _, _) -> pure ()
      (Left err, _) -> assertFailure $ errorBundlePretty err
  where
    fullPath = fixtureRoot <> "/" <> relPath

fixtureRoot :: FilePath
fixtureRoot = "test/fixtures/nixfmt"

validFixtures :: [FilePath]
validFixtures =
  [ "correct/standalone-comments.nix",
    "correct/blank-line-in-interpolation.nix",
    "correct/indented-string.nix",
    "correct/string-with-single-quote-at-end.nix",
    "correct/paths-with-interpolations.nix",
    "correct/quotes-in-inherit.nix",
    "correct/if-with-comments.nix",
    "correct/commented-list.nix",
    "correct/commented-params-list.nix",
    "correct/final-comments-in-sets.nix",
    "diff/comment/in.nix",
    "diff/string/in.nix",
    "diff/string_interpol/in.nix",
    "diff/leading_blank/in.nix",
    "diff/inherit_comment/in.nix",
    "diff/strip_space/in.nix"
  ]

invalidFixtures :: [FilePath]
invalidFixtures =
  [ "invalid/naked-interpolation.nix",
    "invalid/path-starting-with-interpolation.nix",
    "invalid/path-with-interpolation-before-slash.nix",
    "invalid/path-with-escaped-interpolation.nix",
    "invalid/interpolation-in-env-path.nix",
    "invalid/smiley.nix"
  ]

acceptedInvalidFixtures :: [FilePath]
acceptedInvalidFixtures =
  [ "invalid/interpolation-in-inherit-1.nix",
    "invalid/interpolation-in-inherit-2.nix"
  ]
