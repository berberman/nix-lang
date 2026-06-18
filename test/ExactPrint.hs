module ExactPrint where

import Data.Generics (everywhere, mkT)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Nix.Lang.Annotation
import Nix.Lang.ExactPrint
import Nix.Lang.ExactPrint.Edit
import qualified Nix.Lang.ExactPrint.Operations as Ops
import Nix.Lang.Parser
import Nix.Lang.Span
import Nix.Lang.Types
import Nix.Lang.Utils
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (eof, errorBundlePretty)
import Utils

tests :: TestTree
tests =
  testGroup
    "exact print"
    [ testGroup
        "state and layout"
        [ testCase "line comments are collected" lineCommentsCollected,
          testCase "block comments are collected" blockCommentsCollected,
          testCase "file-leading comments attach to root node prior comments" fileLeadingCommentsAttachToRoot,
          testCase "file-trailing comments attach to root node following comments" fileTrailingCommentsAttachToRoot,
          testCase "list carries tokenized delimiter annotations" listCarriesTokenizedDelimiters,
          testCase "list layout prep rewrites delimiters to deltas" listLayoutPrepRewritesDelimitersToDeltas,
          testCase "paren layout prep rewrites close delimiter to delta" parenLayoutPrepRewritesCloseDelimiterToDelta,
          testCase "set layout prep rewrites close delimiter to delta" setLayoutPrepRewritesCloseDelimiterToDelta,
          testCase "insert set binding appends inline binding" insertSetBindingAppendsInlineBinding,
          testCase "insert set binding appends multiline binding" insertSetBindingAppendsMultilineBinding,
          testCase "replace set binding keeps multiline set renderable" replaceSetBindingKeepsMultilineSetRenderable,
          testCase "insert set binding rejects non-set expression" insertSetBindingRejectsNonSet,
          testCase "repair set layout preserves multiline set" repairSetLayoutPreservesMultilineSet,
          testCase "repair let layout preserves multiline let" repairLetLayoutPreservesMultilineLet,
          testCase "repair list layout preserves multiline list" repairListLayoutPreservesMultilineList,
          testCase "repair list layout rejects non-list" repairListLayoutRejectsNonList,
          testCase "repair expr layout supports global integer rewrite in set" repairExprLayoutRepairsGlobalIntegerRewriteInSet,
          testCase "repair expr layout supports global integer rewrite in list" repairExprLayoutRepairsGlobalIntegerRewriteInList,
          testCase "repair expr layout supports shape-changing binding rewrite" repairExprLayoutRepairsShapeChangingBindingRewrite,
  testCase "parse binding parses standalone binding" parseBindingParsesStandaloneBinding,
  testCase "parse expr parses standalone expression" parseExprParsesStandaloneExpression,
  testCase "insert binding appends let binding" insertBindingAppendsLetBinding,
  testCase "replace binding updates set binding" replaceBindingUpdatesSetBinding,
          testCase "insert binding at appends let binding" insertBindingAtAppendsLetBinding,
          testCase "delete binding at removes set binding" deleteBindingAtRemovesSetBinding,
  testCase "replace binding value node at updates set binding" replaceBindingValueNodeAtUpdatesSetBinding,
          testCase "delete binding at removes let binding" deleteBindingAtRemovesLetBinding,
          testCase "replace binding value updates set binding" replaceBindingValueUpdatesSetBinding,
          testCase "replace binding value updates let binding" replaceBindingValueUpdatesLetBinding,
          testCase "replace binding value rejects inherit binding" replaceBindingValueRejectsInheritBinding,
  testCase "insert list element appends multiline list" insertListElementAppendsMultilineList,
  testCase "replace list element updates inline list" replaceListElementUpdatesInlineList,
          testCase "insert list element appends inline list" insertListElementAtAppendsInlineList,
          testCase "insert list element appends multiline list" insertListElementAtAppendsMultilineList,
          testCase "delete list element removes element" deleteListElementAtRemovesElement,
          testCase "rename attr path key renames nested key in set" renameAttrPathKeyRenamesNestedKeyInSet,
          testCase "rename attr path key renames nested key in let" renameAttrPathKeyRenamesNestedKeyInLet,
          testCase "let layout prep rewrites in token to delta" letLayoutPrepRewritesInTokenToDelta,
          testCase "if layout prep rewrites then/else tokens to deltas" ifLayoutPrepRewritesThenElseTokensToDeltas,
          testCase "with layout prep rewrites semicolon to delta" withLayoutPrepRewritesSemicolonToDelta,
          testCase "assert layout prep rewrites semicolon to delta" assertLayoutPrepRewritesSemicolonToDelta,
          testCase "has-attr layout prep rewrites question token to delta" hasAttrLayoutPrepRewritesQuestionToDelta,
          testCase "select layout prep rewrites or token to delta" selectLayoutPrepRewritesOrTokenToDelta,
          testCase "deltaFromAnchor same line computes column delta" deltaFromAnchorSameLine,
          testCase "deltaFromAnchor multiline computes line and indentation" deltaFromAnchorMultiline,
          testCase "renderGapFromDeltaText renders whitespace" renderGapFromDeltaTextRendersWhitespace,
          testCase "advanceCursor tracks newlines" advanceCursorTracksNewlines,
          testCase "assert carries tokenized keyword and semicolon annotations" assertCarriesTokenizedDelimiters,
          testCase "set carries tokenized brace annotations" setCarriesTokenizedBraces,
          testCase "assert carries typed keyword and semicolon spans" assertAnnotationsCollected,
          testCase "set carries typed annotation comments" setCarriesTypedAnnComments,
          testCase "isSubspanOf checks end columns" isSubspanOfChecksEndColumns
        ],
      testGroup
        "roundtrip"
        [ testCase "exact print roundtrips quoted string source" exactPrintRoundtripsQuotedStringSource,
          testCase "exact print roundtrips env path" exactPrintRoundtripsEnvPath,
          testCase "exact print roundtrips multiline list" exactPrintRoundtripsMultilineList,
          testCase "exact print roundtrips multiline set" exactPrintRoundtripsMultilineSet,
          testCase "exact print roundtrips multiline let" exactPrintRoundtripsMultilineLet,
          testCase "exact print roundtrips multiline if" exactPrintRoundtripsMultilineIf,
          testCase "exact print roundtrips multiline with" exactPrintRoundtripsMultilineWith,
          testCase "exact print roundtrips multiline assert" exactPrintRoundtripsMultilineAssert,
          testCase "exact print roundtrips multiline has-attr" exactPrintRoundtripsMultilineHasAttr,
          testCase "exact print roundtrips multiline select or" exactPrintRoundtripsMultilineSelectOr,
          testCase "exact print roundtrips sample fixture" exactPrintRoundtripsSampleFixture,
          testCase "exact print preserves lambda comments from sample shape" exactPrintPreservesLambdaCommentShape,
          testCase "exact print preserves inline close comment without whitespace" exactPrintPreservesCommentNoWhitespace,
          testCase "exact print preserves trailing comma set pattern" exactPrintPreservesTrailingCommaSetPattern,
          testCase "exact print preserves interpolated select attr path" exactPrintPreservesInterpolatedSelectAttrPath,
          testCase "exact print safe API preserves trailing as-pattern" exactPrintSafeApiPreservesTrailingAsPattern,
          testCase "exact print preserves quoted string source text" exactPrintPreservesQuotedStringSource,
          testCase "exact print preserves indented string source text" exactPrintPreservesIndentedStringSource,
          testCase "exact print preserves literal path" exactPrintPreservesLiteralPath,
          testCase "exact print preserves interpolated path structurally" exactPrintPreservesInterpolatedPath,
          testCase "exact print preserves env path" exactPrintPreservesEnvPath,
          testCase "exact print preserves dynamic attr binding" exactPrintPreservesDynamicAttrBinding,
          testCase "exact print preserves dynamic string attr binding" exactPrintPreservesDynamicStringAttrBinding,
          testCase "exact print preserves structural dynamic interpol attr binding" exactPrintPreservesStructuralDynamicInterpolAttrBinding,
          testCase "exact print preserves inherit binding" exactPrintPreservesInheritBinding,
          testCase "exact print preserves lambda var pattern" exactPrintPreservesLambdaVarPattern,
          testCase "exact print preserves lambda set pattern" exactPrintPreservesLambdaSetPattern,
          testCase "exact print preserves multiline lambda chain layout" exactPrintPreservesMultilineLambdaChainLayout,
          testCase "exact print preserves application" exactPrintPreservesApplication,
          testCase "exact print preserves has-attr" exactPrintPreservesHasAttr,
          testCase "exact print preserves with expression" exactPrintPreservesWithExpr,
          testCase "exact print preserves assert expression" exactPrintPreservesAssertExpr,
          testCase "exact print emits file leading and trailing comments" exactPrintEmitsRootComments,
          testCase "exact print emits list trailing comments before close" exactPrintEmitsClosingDelimitedComments,
          testCase "exact print preserves multiline list layout" exactPrintPreservesMultilineListLayout,
          testCase "exact print preserves multiline set layout" exactPrintPreservesMultilineSetLayout,
          testCase "exact print preserves multiline let layout" exactPrintPreservesMultilineLetLayout,
          testCase "exact print preserves multiline if layout" exactPrintPreservesMultilineIfLayout
        ]
    ]

assertExactPrintRoundtripExpr :: T.Text -> Assertion
assertExactPrintRoundtripExpr src =
  case runNixParser (nixExpr <* eof) "<expr>" src of
    (Right expr, _) -> renderExactText expr @?= src
    (Left err, _) -> assertFailure $ errorBundlePretty err

assertExactPrintRoundtripFile :: FilePath -> T.Text -> Assertion
assertExactPrintRoundtripFile fp src =
  case runNixParser nixFile fp src of
    (Right expr, _) -> renderExactText expr @?= src
    (Left err, _) -> assertFailure $ errorBundlePretty err

assertExactPrintMatches :: FilePath -> T.Text -> Assertion
assertExactPrintMatches fp src =
  case runNixParser nixFile fp src of
    (Right expr, _) -> renderExactText expr @?= src
    (Left err, _) -> assertFailure $ errorBundlePretty err

lineCommentsCollected :: Assertion
lineCommentsCollected = do
  expr <- parseFileOrFail "<comments>" "{\n  # hello\n  x = 1;\n}"
  case expr of
    NixSet _ _ (L _ [L _ (NixNormalBinding _ (L _ (NixAttrPath ann _)) _)]) ->
      assertBool "expected collected line comment on first attr path" $ any isHelloLine (priorComments (acComments (aapCommon ann)))
    _ -> assertFailure $ "expected set expression, got: " <> show expr
  where
    isHelloLine (L _ (LineComment txt)) = txt == " hello"
    isHelloLine _ = False

blockCommentsCollected :: Assertion
blockCommentsCollected = do
  expr <- parseFileOrFail "<comments>" "[ a /* hello */ ]"
  case expr of
    NixList ann _ -> assertBool "expected collected block comment before closing bracket" $ any isHelloBlock (followingComments (acComments (alnCommon ann)))
    _ -> assertFailure $ "expected list expression, got: " <> show expr
  where
    isHelloBlock (L _ (BlockComment txt)) = txt == " hello "
    isHelloBlock _ = False

fileLeadingCommentsAttachToRoot :: Assertion
fileLeadingCommentsAttachToRoot = do
  expr <- parseFileOrFail "<comments>" "# hello\n1"
  case expr of
    NixLit ann _ -> assertBool "expected file-leading comment on root prior comments" $ any isHelloLine (priorComments (acComments ann))
    _ -> assertFailure $ "expected literal expression, got: " <> show expr
  where
    isHelloLine (L _ (LineComment txt)) = txt == " hello"
    isHelloLine _ = False

fileTrailingCommentsAttachToRoot :: Assertion
fileTrailingCommentsAttachToRoot = do
  expr <- parseFileOrFail "<comments>" "1\n# hello"
  case expr of
    NixLit ann _ -> assertBool "expected file-trailing comment on root following comments" $ any isHelloLine (followingComments (acComments ann))
    _ -> assertFailure $ "expected literal expression, got: " <> show expr
  where
    isHelloLine (L _ (LineComment txt)) = txt == " hello"
    isHelloLine _ = False

listCarriesTokenizedDelimiters :: Assertion
listCarriesTokenizedDelimiters = do
  expr <- parseExprOrFail "[1 2 3]"
  case expr of
    NixList ann _ -> do
      annToken (alnOpenS ann) @?= AnnOpenS
      annToken (alnCloseS ann) @?= AnnCloseS
      annTokenSrcSpan (alnOpenS ann) @?= Just (mkSrcSpan "<expr>" (1, 1) (1, 2))
      annTokenSrcSpan (alnCloseS ann) @?= Just (mkSrcSpan "<expr>" (1, 7) (1, 8))
    _ -> assertFailure $ "expected list expression, got: " <> show expr

listLayoutPrepRewritesDelimitersToDeltas :: Assertion
listLayoutPrepRewritesDelimitersToDeltas = do
  expr <- parseExprOrFail "[\n  a\n  b\n]"
  case expr of
    NixList ann xs -> do
      let ann' = Ops.prepareListLayout ann xs
      annTokenSrcSpan (alnOpenS ann') @?= annTokenSrcSpan (alnOpenS ann)
      annTokenDelta (alnCloseS ann') @?= Just (DeltaPos 1 0)
      annTokenSrcSpan (alnCloseS ann') @?= Nothing
    _ -> assertFailure $ "expected list expression, got: " <> show expr

parenLayoutPrepRewritesCloseDelimiterToDelta :: Assertion
parenLayoutPrepRewritesCloseDelimiterToDelta = do
  expr <- parseExprOrFail "(\n  1\n)"
  case expr of
    NixPar ann inner -> do
      let ann' = Ops.prepareParLayout ann (unLoc inner)
      annTokenSrcSpan (apnOpenP ann') @?= annTokenSrcSpan (apnOpenP ann)
      annTokenDelta (apnCloseP ann') @?= Just (DeltaPos 1 0)
      annTokenSrcSpan (apnCloseP ann') @?= Nothing
    _ -> assertFailure $ "expected parenthesized expression, got: " <> show expr

setLayoutPrepRewritesCloseDelimiterToDelta :: Assertion
setLayoutPrepRewritesCloseDelimiterToDelta = do
  expr <- parseExprOrFail "{\n  x = 1;\n}"
  case expr of
    NixSet ann _ (L _ bindings) -> do
      let ann' = Ops.prepareSetLayout ann bindings
      annTokenSrcSpan (asOpenC ann') @?= annTokenSrcSpan (asOpenC ann)
      annTokenDelta (asCloseC ann') @?= Just (DeltaPos 1 0)
      annTokenSrcSpan (asCloseC ann') @?= Nothing
    _ -> assertFailure $ "expected set expression, got: " <> show expr

insertSetBindingAppendsInlineBinding :: Assertion
insertSetBindingAppendsInlineBinding = do
  expr <- parseExprOrFail "{ }"
  case insertSetBindingText AppendBinding "x = 1;" expr of
    Right edited -> renderExactText edited @?= "{ x = 1; }"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

insertSetBindingAppendsMultilineBinding :: Assertion
insertSetBindingAppendsMultilineBinding = do
  expr <- parseExprOrFail "{\n  a = 1;\n}"
  case insertSetBindingText AppendBinding "b = 2;" expr of
    Right edited -> renderExactText edited @?= "{\n  a = 1;\n  b = 2;\n}"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

replaceSetBindingKeepsMultilineSetRenderable :: Assertion
replaceSetBindingKeepsMultilineSetRenderable = do
  expr <- parseExprOrFail "{\n  a = 1;\n}"
  case replaceSetBindingText 0 "config.allowUnfree = true;" expr of
    Right edited -> renderExactText edited @?= "{\n  config.allowUnfree = true;\n}"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

insertSetBindingRejectsNonSet :: Assertion
insertSetBindingRejectsNonSet = do
  expr <- parseExprOrFail "1"
  case insertSetBindingText AppendBinding "x = 1;" expr of
    Left NotASet -> pure ()
    other -> assertFailure $ "expected Left NotASet, got: " <> show other

repairSetLayoutPreservesMultilineSet :: Assertion
repairSetLayoutPreservesMultilineSet = do
  expr <- parseExprOrFail "{\n  a = 1;\n  b = 2;\n}"
  case repairSetLayout expr of
    Right edited -> renderExactText edited @?= "{\n  a = 1;\n  b = 2;\n}"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

repairLetLayoutPreservesMultilineLet :: Assertion
repairLetLayoutPreservesMultilineLet = do
  expr <- parseExprOrFail "let\n  a = 1;\n  b = 2;\nin\n  a"
  case repairLetLayout expr of
    Right edited -> renderExactText edited @?= "let\n  a = 1;\n  b = 2;\nin\n  a"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

repairListLayoutPreservesMultilineList :: Assertion
repairListLayoutPreservesMultilineList = do
  expr <- parseExprOrFail "[\n  a\n  b\n]"
  case repairListLayout expr of
    Right edited -> renderExactText edited @?= "[\n  a\n  b\n]"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

repairListLayoutRejectsNonList :: Assertion
repairListLayoutRejectsNonList = do
  expr <- parseExprOrFail "1"
  case repairListLayout expr of
    Left NotAList -> pure ()
    other -> assertFailure $ "expected Left NotAList, got: " <> show other

repairExprLayoutRepairsGlobalIntegerRewriteInSet :: Assertion
repairExprLayoutRepairsGlobalIntegerRewriteInSet = do
  expr <- parseExprOrFail "{\n  a = 1;\n  b = [\n    2\n    3\n  ];\n}"
  let mutated = everywhere (mkT incrementIntegerLiteral) expr
  case repairExprLayout mutated of
    Right repaired -> renderExactText repaired @?= "{\n  a = 2;\n  b = [\n    3\n    4\n  ];\n}"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

repairExprLayoutRepairsGlobalIntegerRewriteInList :: Assertion
repairExprLayoutRepairsGlobalIntegerRewriteInList = do
  expr <- parseExprOrFail "[\n  1\n  {\n    x = 2;\n  }\n  3\n]"
  let mutated = everywhere (mkT incrementIntegerLiteral) expr
  case repairExprLayout mutated of
    Right repaired -> renderExactText repaired @?= "[\n  2\n  {\n    x = 3;\n  }\n  4\n]"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

repairExprLayoutRepairsShapeChangingBindingRewrite :: Assertion
repairExprLayoutRepairsShapeChangingBindingRewrite = do
  expr <- parseExprOrFail "{\n  a = 1;\n}"
  replacement <- parseExprOrFail "if\n  cond\nthen\n  2\nelse\n  3"
  let mutated = rewriteFirstBindingValue replacement expr
  case repairExprLayout mutated of
    Right repaired ->
      renderExactText repaired
        @?= "{\n  a = if\n  cond\nthen\n  2\nelse\n  3;\n}"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

parseBindingParsesStandaloneBinding :: Assertion
parseBindingParsesStandaloneBinding =
  case parseBinding "config.allowUnfree = true;" of
    Right (L _ (NixNormalBinding _ path value)) -> do
      renderExactText(unLoc path) @?= "config.allowUnfree"
      renderExactText(unLoc value) @?= "true"
    Left err -> assertFailure $ "unexpected parse error: " <> show err
    other -> assertFailure $ "expected normal binding fragment, got: " <> show other

parseExprParsesStandaloneExpression :: Assertion
parseExprParsesStandaloneExpression =
  case parseExpr "a.b or c" of
    Right (L _ expr) -> renderExactText expr @?= "a.b or c"
    Left err -> assertFailure $ "unexpected parse error: " <> show err

insertBindingAppendsLetBinding :: Assertion
insertBindingAppendsLetBinding = do
  expr <- parseExprOrFail "let\n  a = 1;\nin\n  a"
  case parseBinding "b = 2;" of
    Right binding ->
      case insertBinding AppendBinding binding expr of
        Right edited -> renderExactText edited @?= "let\n  a = 1;\n  b = 2;\nin\n  a"
        Left err -> assertFailure $ "unexpected edit error: " <> show err
    Left err -> assertFailure $ "unexpected parse error: " <> show err

replaceBindingUpdatesSetBinding :: Assertion
replaceBindingUpdatesSetBinding = do
  expr <- parseExprOrFail "{\n  a = 1;\n}"
  case parseBinding "config.allowUnfree = true;" of
    Right binding ->
      case replaceBinding 0 binding expr of
        Right edited -> renderExactText edited @?= "{\n  config.allowUnfree = true;\n}"
        Left err -> assertFailure $ "unexpected edit error: " <> show err
    Left err -> assertFailure $ "unexpected parse error: " <> show err

insertBindingAtAppendsLetBinding :: Assertion
insertBindingAtAppendsLetBinding = do
  expr <- parseExprOrFail "let\n  a = 1;\nin\n  a"
  case insertBindingAt AppendBinding "b = 2;" expr of
    Right edited -> renderExactText edited @?= "let\n  a = 1;\n  b = 2;\nin\n  a"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

deleteBindingAtRemovesSetBinding :: Assertion
deleteBindingAtRemovesSetBinding = do
  expr <- parseExprOrFail "{\n  a = 1;\n  b = 2;\n}"
  case deleteBindingAt 0 expr of
    Right edited -> renderExactText edited @?= "{\n  b = 2;\n}"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

deleteBindingAtRemovesLetBinding :: Assertion
deleteBindingAtRemovesLetBinding = do
  expr <- parseExprOrFail "let\n  a = 1;\n  b = 2;\nin\n  b"
  case deleteBindingAt 0 expr of
    Right edited -> renderExactText edited @?= "let\n  b = 2;\nin\n  b"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

replaceBindingValueNodeAtUpdatesSetBinding :: Assertion
replaceBindingValueNodeAtUpdatesSetBinding = do
  expr <- parseExprOrFail "{\n  a = 1;\n}"
  case parseExpr "b" of
    Right value ->
      case replaceBindingValueNodeAt 0 value expr of
        Right edited -> renderExactText edited @?= "{\n  a = b;\n}"
        Left err -> assertFailure $ "unexpected edit error: " <> show err
    Left err -> assertFailure $ "unexpected parse error: " <> show err

replaceBindingValueUpdatesSetBinding :: Assertion
replaceBindingValueUpdatesSetBinding = do
  expr <- parseExprOrFail "{\n  a = 1;\n}"
  case replaceBindingValue 0 "b" expr of
    Right edited -> renderExactText edited @?= "{\n  a = b;\n}"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

replaceBindingValueUpdatesLetBinding :: Assertion
replaceBindingValueUpdatesLetBinding = do
  expr <- parseExprOrFail "let\n  a = 1;\nin\n  a"
  case replaceBindingValue 0 "b" expr of
    Right edited -> renderExactText edited @?= "let\n  a = b;\nin\n  a"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

replaceBindingValueRejectsInheritBinding :: Assertion
replaceBindingValueRejectsInheritBinding = do
  expr <- parseExprOrFail "{ inherit a; }"
  case replaceBindingValue 0 "b" expr of
    Left (NotANormalBinding 0) -> pure ()
    other -> assertFailure $ "expected Left (NotANormalBinding 0), got: " <> show other

insertListElementAppendsMultilineList :: Assertion
insertListElementAppendsMultilineList = do
  expr <- parseExprOrFail "[\n  a\n]"
  case parseExpr "b" of
    Right element ->
      case insertListElement AppendListElement element expr of
        Right edited -> renderExactText edited @?= "[\n  a\n  b\n]"
        Left err -> assertFailure $ "unexpected edit error: " <> show err
    Left err -> assertFailure $ "unexpected parse error: " <> show err

replaceListElementUpdatesInlineList :: Assertion
replaceListElementUpdatesInlineList = do
  expr <- parseExprOrFail "[ a b ]"
  case parseExpr "c" of
    Right element ->
      case replaceListElement 1 element expr of
        Right edited -> renderExactText edited @?= "[ a c ]"
        Left err -> assertFailure $ "unexpected edit error: " <> show err
    Left err -> assertFailure $ "unexpected parse error: " <> show err

insertListElementAtAppendsInlineList :: Assertion
insertListElementAtAppendsInlineList = do
  expr <- parseExprOrFail "[ ]"
  case insertListElementAt AppendListElement "a" expr of
    Right edited -> renderExactText edited @?= "[ a ]"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

insertListElementAtAppendsMultilineList :: Assertion
insertListElementAtAppendsMultilineList = do
  expr <- parseExprOrFail "[\n  a\n]"
  case insertListElementAt AppendListElement "b" expr of
    Right edited -> renderExactText edited @?= "[\n  a\n  b\n]"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

deleteListElementAtRemovesElement :: Assertion
deleteListElementAtRemovesElement = do
  expr <- parseExprOrFail "[\n  a\n  b\n]"
  case deleteListElementAt 0 expr of
    Right edited -> renderExactText edited @?= "[\n  b\n]"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

renameAttrPathKeyRenamesNestedKeyInSet :: Assertion
renameAttrPathKeyRenamesNestedKeyInSet = do
  expr <- parseExprOrFail "{\n  foo.bar = 1;\n}"
  case renameAttrPathKey 0 1 "baz" expr of
    Right edited -> renderExactText edited @?= "{\n  foo.baz = 1;\n}"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

renameAttrPathKeyRenamesNestedKeyInLet :: Assertion
renameAttrPathKeyRenamesNestedKeyInLet = do
  expr <- parseExprOrFail "let\n  foo.bar = 1;\nin\n  foo.bar"
  case renameAttrPathKey 0 1 "baz" expr of
    Right edited -> renderExactText edited @?= "let\n  foo.baz = 1;\nin\n  foo bar"
    Left err -> assertFailure $ "unexpected edit error: " <> show err

incrementIntegerLiteral :: NixLit Ps -> NixLit Ps
incrementIntegerLiteral = \case
  NixInteger ann n -> NixInteger ann (n + 1)
  lit -> lit

rewriteFirstBindingValue :: NixExpr Ps -> NixExpr Ps -> NixExpr Ps
rewriteFirstBindingValue replacement = \case
  NixSet ann kind (L bindingsLoc (binding : rest)) ->
    NixSet ann kind (L bindingsLoc (rewriteBinding binding : rest))
  expr -> expr
  where
    rewriteBinding (L bindingLoc binding) =
      case binding of
        NixNormalBinding ann path expr ->
          L bindingLoc (NixNormalBinding ann path (L (getLoc expr) replacement))
        other -> L bindingLoc other

letLayoutPrepRewritesInTokenToDelta :: Assertion
letLayoutPrepRewritesInTokenToDelta = do
  expr <- parseExprOrFail "let\n  x = 1;\nin\n  x"
  case expr of
    NixLet ann (L _ bindings) body -> do
      let ann' = Ops.prepareLetLayout ann bindings (unLoc body)
      annTokenDelta (alIn ann') @?= Just (DeltaPos 1 0)
      annTokenSrcSpan (alIn ann') @?= Nothing
    _ -> assertFailure $ "expected let expression, got: " <> show expr

ifLayoutPrepRewritesThenElseTokensToDeltas :: Assertion
ifLayoutPrepRewritesThenElseTokensToDeltas = do
  expr <- parseExprOrFail "if\n  a\nthen\n  b\nelse\n  c"
  case expr of
    NixIf ann cond thenExpr elseExpr -> do
      let ann' = Ops.prepareIfLayout ann (unLoc cond) (unLoc thenExpr) (unLoc elseExpr)
      annTokenDelta (aifThen ann') @?= Just (DeltaPos 1 0)
      annTokenDelta (aifElse ann') @?= Just (DeltaPos 1 0)
      annTokenSrcSpan (aifThen ann') @?= Nothing
      annTokenSrcSpan (aifElse ann') @?= Nothing
    _ -> assertFailure $ "expected if expression, got: " <> show expr

withLayoutPrepRewritesSemicolonToDelta :: Assertion
withLayoutPrepRewritesSemicolonToDelta = do
  expr <- parseExprOrFail "with\n  l\n;\n  1"
  case expr of
    NixWith ann scope body -> do
      let ann' = Ops.prepareWithLayout ann (unLoc scope) (unLoc body)
      annTokenDelta (awSemicolon ann') @?= Just (DeltaPos 1 0)
      annTokenSrcSpan (awSemicolon ann') @?= Nothing
    _ -> assertFailure $ "expected with expression, got: " <> show expr

assertLayoutPrepRewritesSemicolonToDelta :: Assertion
assertLayoutPrepRewritesSemicolonToDelta = do
  expr <- parseExprOrFail "assert\n  a == 1\n;\n  2"
  case expr of
    NixAssert ann assertion body -> do
      let ann' = Ops.prepareAssertLayout ann (unLoc assertion) (unLoc body)
      annTokenDelta (aaSemicolon ann') @?= Just (DeltaPos 1 0)
      annTokenSrcSpan (aaSemicolon ann') @?= Nothing
    _ -> assertFailure $ "expected assert expression, got: " <> show expr

hasAttrLayoutPrepRewritesQuestionToDelta :: Assertion
hasAttrLayoutPrepRewritesQuestionToDelta = do
  expr <- parseExprOrFail "a\n?\n  b"
  case expr of
    NixHasAttr ann lhs rhs -> do
      let ann' = Ops.prepareHasAttrLayout ann (unLoc lhs) (unLoc rhs)
      annTokenDelta (ahaQuestion ann') @?= Just (DeltaPos 1 0)
      annTokenSrcSpan (ahaQuestion ann') @?= Nothing
    _ -> assertFailure $ "expected has-attr expression, got: " <> show expr

selectLayoutPrepRewritesOrTokenToDelta :: Assertion
selectLayoutPrepRewritesOrTokenToDelta = do
  expr <- parseExprOrFail "a.b.c\nor\n  d.e"
  case expr of
    NixSelect ann lhs path (Just defExpr) -> do
      let ann' = Ops.prepareSelectLayout ann (unLoc lhs) (unLoc path) (Just defExpr)
      case aslOr ann' of
        Just tok -> do
          annTokenDelta tok @?= Just (DeltaPos 1 0)
          annTokenSrcSpan tok @?= Nothing
        Nothing -> assertFailure "expected or token"
    _ -> assertFailure $ "expected select expression, got: " <> show expr

deltaFromAnchorSameLine :: Assertion
deltaFromAnchorSameLine =
  Ops.deltaFromAnchor (mkSrcSpan "<test>" (1, 1) (1, 4)) (mkSrcSpan "<test>" (1, 7) (1, 8)) @?= DeltaPos 0 3

deltaFromAnchorMultiline :: Assertion
deltaFromAnchorMultiline =
  Ops.deltaFromAnchor (mkSrcSpan "<test>" (1, 3) (1, 8)) (mkSrcSpan "<test>" (3, 5) (3, 6)) @?= DeltaPos 2 4

renderGapFromDeltaTextRendersWhitespace :: Assertion
renderGapFromDeltaTextRendersWhitespace = do
  Ops.renderGapFromDeltaText (DeltaPos 0 3) @?= "   "
  Ops.renderGapFromDeltaText (DeltaPos 2 4) @?= "\n\n    "

advanceCursorTracksNewlines :: Assertion
advanceCursorTracksNewlines =
  Ops.advanceCursor (Ops.RenderCursor 1 1) "ab\n c" @?= Ops.RenderCursor 2 3

assertAnnotationsCollected :: Assertion
assertAnnotationsCollected = do
  expr <- parseExprOrFail "assert a == 1; 2"
  case expr of
    NixAssert ann _ _ -> do
      fmap srcSpanStartColumn (annTokenSrcSpan (aaAssert ann)) @?= Just 1
      fmap srcSpanStartColumn (annTokenSrcSpan (aaSemicolon ann)) @?= Just 14
    _ -> assertFailure $ "expected assert expression, got: " <> show expr

assertCarriesTokenizedDelimiters :: Assertion
assertCarriesTokenizedDelimiters = do
  expr <- parseExprOrFail "assert a == 1; 2"
  case expr of
    NixAssert ann _ _ -> do
      annToken (aaAssert ann) @?= AnnAssert
      annToken (aaSemicolon ann) @?= AnnSemicolon
      annTokenSrcSpan (aaAssert ann) @?= Just (mkSrcSpan "<expr>" (1, 1) (1, 7))
      annTokenSrcSpan (aaSemicolon ann) @?= Just (mkSrcSpan "<expr>" (1, 14) (1, 15))
    _ -> assertFailure $ "expected assert expression, got: " <> show expr

setCarriesTokenizedBraces :: Assertion
setCarriesTokenizedBraces = do
  expr <- parseExprOrFail "{ x = 1; }"
  case expr of
    NixSet ann _ _ -> do
      annToken (asOpenC ann) @?= AnnOpenC
      annToken (asCloseC ann) @?= AnnCloseC
      annTokenSrcSpan (asOpenC ann) @?= Just (mkSrcSpan "<expr>" (1, 1) (1, 2))
      annTokenSrcSpan (asCloseC ann) @?= Just (mkSrcSpan "<expr>" (1, 10) (1, 11))
    _ -> assertFailure $ "expected set expression, got: " <> show expr

setCarriesTypedAnnComments :: Assertion
setCarriesTypedAnnComments = do
  expr <- parseFileOrFail "<comments>" "{\n  # hello\n  x = 1;\n}"
  case expr of
    NixSet ann _ _ -> do
      fmap srcSpanStartColumn (annTokenSrcSpan (asOpenC ann)) @?= Just 1
      fmap srcSpanStartColumn (annTokenSrcSpan (asCloseC ann)) @?= Just 1
      acComments (asCommon ann) @?= emptyComments
    _ -> assertFailure $ "expected set expression, got: " <> show expr

isSubspanOfChecksEndColumns :: Assertion
isSubspanOfChecksEndColumns = do
  let parent = mkSrcSpan "<test>" (1, 1) (1, 5)
      inside = mkSrcSpan "<test>" (1, 2) (1, 5)
      outside = mkSrcSpan "<test>" (1, 2) (1, 6)
  assertBool "subspan ending at parent column should be inside" $ inside `isSubspanOf` parent
  assertBool "subspan ending after parent column should be outside" $ not (outside `isSubspanOf` parent)

exactPrintPreservesQuotedStringSource :: Assertion
exactPrintPreservesQuotedStringSource = do
  case runNixParser (nixExpr <* eof) "<expr>" "\"a$${b}c\"" of
    (Right expr, _) -> renderExactText expr @?= "\"a$${b}c\""
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintRoundtripsQuotedStringSource :: Assertion
exactPrintRoundtripsQuotedStringSource = assertExactPrintRoundtripExpr "\"a$${b}c\""

exactPrintRoundtripsEnvPath :: Assertion
exactPrintRoundtripsEnvPath = assertExactPrintRoundtripExpr "<nixpkgs/nixos>"

exactPrintRoundtripsMultilineList :: Assertion
exactPrintRoundtripsMultilineList = assertExactPrintRoundtripFile "<list>" "[\n  a\n  b\n]"

exactPrintRoundtripsMultilineSet :: Assertion
exactPrintRoundtripsMultilineSet = assertExactPrintRoundtripFile "<set>" "{\n  x = 1;\n  y = 2;\n}"

exactPrintRoundtripsMultilineLet :: Assertion
exactPrintRoundtripsMultilineLet = assertExactPrintRoundtripFile "<let>" "let\n  x = 1;\nin\n  x"

exactPrintRoundtripsMultilineIf :: Assertion
exactPrintRoundtripsMultilineIf = assertExactPrintRoundtripFile "<if>" "if\n  a\nthen\n  b\nelse\n  c"

exactPrintRoundtripsMultilineWith :: Assertion
exactPrintRoundtripsMultilineWith = assertExactPrintRoundtripFile "<with>" "with\n  l\n;\n  1"

exactPrintRoundtripsMultilineAssert :: Assertion
exactPrintRoundtripsMultilineAssert = assertExactPrintRoundtripFile "<assert>" "assert\n  a == 1\n;\n  2"

exactPrintRoundtripsMultilineHasAttr :: Assertion
exactPrintRoundtripsMultilineHasAttr = assertExactPrintRoundtripFile "<has-attr>" "a\n?\n  b"

exactPrintRoundtripsMultilineSelectOr :: Assertion
exactPrintRoundtripsMultilineSelectOr = assertExactPrintRoundtripFile "<select>" "a.b.c\nor\n  d.e"

exactPrintRoundtripsSampleFixture :: Assertion
exactPrintRoundtripsSampleFixture = do
  src <- T.readFile "test/sample.nix"
  assertExactPrintRoundtripFile "test/sample.nix" (maybe src id (T.stripSuffix "\n" src))

exactPrintPreservesLambdaCommentShape :: Assertion
exactPrintPreservesLambdaCommentShape =
  assertExactPrintMatches
    "<lambda-comments>"
    "let\n  # comment f\n  f =\n    x:\n    # comment y\n    y:\n    x;\nin f"

exactPrintPreservesCommentNoWhitespace :: Assertion
exactPrintPreservesCommentNoWhitespace =
  assertExactPrintMatches "<comment-no-ws>" "{ comment_no_ws = [a/**/]; }"

exactPrintPreservesTrailingCommaSetPattern :: Assertion
exactPrintPreservesTrailingCommaSetPattern =
  assertExactPrintMatches "<param-comma>" "{x,}: x"

exactPrintPreservesInterpolatedSelectAttrPath :: Assertion
exactPrintPreservesInterpolatedSelectAttrPath =
  assertExactPrintMatches "<sel-interpol>" "a.b.${c}.\"d\".\"${\"e\"}\""

exactPrintSafeApiPreservesTrailingAsPattern :: Assertion
exactPrintSafeApiPreservesTrailingAsPattern = do
  case runNixParser nixFile "<lambda>" "{x}@a: x" of
    (Right expr, _) -> renderExactTextM expr @?= Right "{x}@a: x"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesIndentedStringSource :: Assertion
exactPrintPreservesIndentedStringSource = do
  case runNixParser (nixExpr <* eof) "<expr>" "''a${b}c''" of
    (Right expr, _) -> renderExactText expr @?= "''a${b}c''"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesLiteralPath :: Assertion
exactPrintPreservesLiteralPath = do
  case runNixParser (nixExpr <* eof) "<expr>" "./nix" of
    (Right expr, _) -> renderExactText expr @?= "./nix"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesInterpolatedPath :: Assertion
exactPrintPreservesInterpolatedPath = do
  case runNixParser (nixExpr <* eof) "<expr>" "./${a}-${b}/c/d${e}" of
    (Right expr, _) -> renderExactText expr @?= "./${a}-${b}/c/d${e}"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesEnvPath :: Assertion
exactPrintPreservesEnvPath = do
  case runNixParser (nixExpr <* eof) "<expr>" "<nixpkgs/nixos>" of
    (Right expr, _) -> renderExactText expr @?= "<nixpkgs/nixos>"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesDynamicAttrBinding :: Assertion
exactPrintPreservesDynamicAttrBinding = do
  case runNixParser nixFile "<set>" "{ \"${dynamic}\".${attr} = g; }" of
    (Right expr, _) -> renderExactText expr @?= "{ \"${dynamic}\".${attr} = g; }"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesDynamicStringAttrBinding :: Assertion
exactPrintPreservesDynamicStringAttrBinding = do
  case runNixParser nixFile "<set>" "{ \"a${b}c\" = g; }" of
    (Right expr, _) -> renderExactText expr @?= "{ \"a${b}c\" = g; }"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesStructuralDynamicInterpolAttrBinding :: Assertion
exactPrintPreservesStructuralDynamicInterpolAttrBinding = do
  case runNixParser nixFile "<set>" "{ ${a + b} = g; }" of
    (Right expr, _) -> renderExactText expr @?= "{ ${a + b} = g; }"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesInheritBinding :: Assertion
exactPrintPreservesInheritBinding = do
  case runNixParser nixFile "<set>" "{ inherit (s) m g; }" of
    (Right expr, _) -> renderExactText expr @?= "{ inherit (s) m g; }"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesLambdaVarPattern :: Assertion
exactPrintPreservesLambdaVarPattern = do
  case runNixParser nixFile "<lambda>" "x: x" of
    (Right expr, _) -> renderExactText expr @?= "x: x"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesLambdaSetPattern :: Assertion
exactPrintPreservesLambdaSetPattern = do
  case runNixParser nixFile "<lambda>" "{ x ? 1, y ? {}, ... }: x" of
    (Right expr, _) -> renderExactText expr @?= "{ x ? 1, y ? {}, ... }: x"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesMultilineLambdaChainLayout :: Assertion
exactPrintPreservesMultilineLambdaChainLayout =
  assertExactPrintMatches "<lambda-multiline>" "x:\n  y:\n  x"

exactPrintPreservesApplication :: Assertion
exactPrintPreservesApplication = do
  case runNixParser nixFile "<app>" "a b c" of
    (Right expr, _) -> renderExactText expr @?= "a b c"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesHasAttr :: Assertion
exactPrintPreservesHasAttr = do
  case runNixParser nixFile "<has-attr>" "a ? b" of
    (Right expr, _) -> renderExactText expr @?= "a ? b"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesWithExpr :: Assertion
exactPrintPreservesWithExpr = do
  case runNixParser nixFile "<with>" "with l; 1" of
    (Right expr, _) -> renderExactText expr @?= "with l; 1"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesAssertExpr :: Assertion
exactPrintPreservesAssertExpr = do
  case runNixParser nixFile "<assert>" "assert a == 1; 2" of
    (Right expr, _) -> renderExactText expr @?= "assert a == 1; 2"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintEmitsRootComments :: Assertion
exactPrintEmitsRootComments = do
  case runNixParser nixFile "<comments>" "# hello\n1\n# bye" of
    (Right expr, _) -> renderExactText expr @?= "# hello\n1\n# bye"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintEmitsClosingDelimitedComments :: Assertion
exactPrintEmitsClosingDelimitedComments = do
  case runNixParser nixFile "<comments>" "[ a /* hello */ ]" of
    (Right expr, _) -> renderExactText expr @?= "[ a /* hello */ ]"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesMultilineListLayout :: Assertion
exactPrintPreservesMultilineListLayout = do
  case runNixParser nixFile "<list>" "[\n  a\n  b\n]" of
    (Right expr, _) -> renderExactText expr @?= "[\n  a\n  b\n]"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesMultilineSetLayout :: Assertion
exactPrintPreservesMultilineSetLayout = do
  case runNixParser nixFile "<set>" "{\n  x = 1;\n  y = 2;\n}" of
    (Right expr, _) -> renderExactText expr @?= "{\n  x = 1;\n  y = 2;\n}"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesMultilineLetLayout :: Assertion
exactPrintPreservesMultilineLetLayout = do
  case runNixParser nixFile "<let>" "let\n  x = 1;\nin\n  x" of
    (Right expr, _) -> renderExactText expr @?= "let\n  x = 1;\nin\n  x"
    (Left err, _) -> assertFailure $ errorBundlePretty err

exactPrintPreservesMultilineIfLayout :: Assertion
exactPrintPreservesMultilineIfLayout = do
  case runNixParser nixFile "<if>" "if\n  a\nthen\n  b\nelse\n  c" of
    (Right expr, _) -> renderExactText expr @?= "if\n  a\nthen\n  b\nelse\n  c"
    (Left err, _) -> assertFailure $ errorBundlePretty err
