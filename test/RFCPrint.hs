{-# LANGUAGE EmptyCase #-}

module RFCPrint where

import qualified Data.Text as T
import Nix.Lang.Parser (nixExpr, runNixParser)
import Nix.Lang.RFCPrint (formatExpr, formatExprWithHints)
import Nix.Lang.RFCPrint.LayoutHints
import Nix.Lang.Types
import Nix.Lang.Types.Syn
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (eof, errorBundlePretty)
import Utils (parseExprOrFail, parseFileOrFail)

tests :: TestTree
tests =
  testGroup
    "rfc print"
    [ testGroup
        "precedence and parentheses"
        [ testCase "formats application argument with required parentheses" applicationArgumentParens,
          testCase "formats application chains" applicationChainFormatting,
          testCase "formats binary precedence correctly" binaryPrecedence,
          testCase "formats right associative concat" concatAssociativity,
          testCase "formats right associative updates" updateAssociativity,
          testCase "formats right associative implication" implicationAssociativity,
          testCase "formats non associative equality with parentheses" equalityParens,
          testCase "formats select default without losing precedence" selectDefaultFormatting,
          testCase "formats inherit scope with parentheses" inheritScopeParens
        ],
      testGroup
        "containers and bindings"
        [ testCase "formats empty list" emptyListFormatting,
          testCase "formats empty set" emptySetFormatting,
          testCase "formats empty recursive set" emptyRecSetFormatting,
          testCase "formats empty let" emptyLetFormatting,
          testCase "formats long lists multiline" longListFormatting,
          testCase "formats multiline inherit bindings" inheritManyFormatting,
          testCase "formats multiline inherit-from bindings" inheritFromManyFormatting,
          testCase "formats nested set binding values multiline" nestedSetBindingFormatting,
          testCase "formats let binding values multiline" letValueBindingFormatting,
          testCase "formats empty set-pattern lambdas with spaced braces" emptySetPatternFormatting,
          testCase "formats ellipsis-only set-pattern lambdas with spaced braces" ellipsisOnlySetPatternFormatting,
          testCase "formats simple set-pattern lambdas multiline with trailing commas" simpleSetPatternFormatting,
          testCase "formats set recursively" recursiveSetFormatting,
          testCase "formats let expression multiline" letFormatting,
          testCase "formats lambda set pattern" lambdaSetPatternFormatting
        ],
      testGroup
        "keyword expressions"
        [ testCase "formats with expression inline when compact" withFormatting,
          testCase "formats with attrset body multiline" withAttrsetFormatting,
          testCase "formats assert expression multiline" assertFormatting,
          testCase "formats assert attrset body multiline" assertAttrsetFormatting,
          testCase "formats if expression inline when compact" ifFormatting,
          testCase "formats else-if chains without indentation creep" elseIfFormatting
        ],
      testGroup
        "application layout"
        [ testCase "formats attrset application arguments multiline" applicationAttrsetFormatting,
          testCase "formats let application arguments multiline" applicationLetFormatting
        ],
      testGroup
        "strings and reparsing"
        [ testCase "formats strings and reparses" stringFormatting
        ],
      testGroup
        "nixfmt fixed point"
        [ testCase "nixfmt is no-op for fresh Syn output" nixfmtIsNoOp
        ],
      testGroup
        "layout hints"
        [ testCase "collects multiline list hint from parsed source" collectsMultilineListHint,
          testCase "parsed source can preserve inline list layout with hints" preservesInlineListWithHints,
          testCase "parsed source can preserve inline let layout with hints" preservesInlineLetWithHints,
          testCase "parsed source can preserve inline interpolation list layout with hints" preservesInlineInterpolationListWithHints,
          testCase "parsed source can preserve inline path interpolation list layout with hints" preservesInlinePathInterpolationListWithHints,
          testCase "parsed source can preserve root leading comments canonically" preservesRootLeadingCommentsWithHints,
          testCase "parsed source can preserve root trailing comments canonically" preservesRootTrailingCommentsWithHints,
          testCase "parsed source can preserve multiple root comments with blank lines canonically" preservesMultipleRootCommentsWithBlankLinesWithHints,
          testCase "parsed source can preserve nested set comments canonically" preservesNestedSetCommentsWithHints,
          testCase "parsed source can preserve blank line before nested binding comments canonically" preservesNestedBlankLineCommentsWithHints,
          testCase "parsed source can preserve blank line before second binding comment canonically" preservesSecondBindingBlankLineCommentsWithHints,
          testCase "parsed source can preserve inherit-binding comments canonically" preservesInheritBindingCommentsWithHints
        ]
    ]

assertFormatsTo :: Expr -> String -> Assertion
assertFormatsTo expr expected = formatExpr expr @?= T.pack expected

assertRoundtrips :: Expr -> Assertion
assertRoundtrips expr = do
  let rendered = formatExpr expr
  case runNixParser (nixExpr <* eof) "<rfc>" rendered of
    (Right actual, _) -> show (stripParensExpr (lowerParsedExpr actual)) @?= show (stripParensExpr expr)
    (Left err, _) -> assertFailure (errorBundlePretty err)

assertNixfmtFixedPoint :: Expr -> Assertion
assertNixfmtFixedPoint expr = do
  let rendered = T.unpack (formatExpr expr) <> "\n"
  (exitCode, stdout, stderr) <- readProcessWithExitCode "nixfmt" [] rendered
  case exitCode of
    ExitSuccess -> stdout @?= rendered
    ExitFailure code -> assertFailure $ unlines ["nixfmt failed with exit code " <> show code, stderr]

applicationArgumentParens :: Assertion
applicationArgumentParens = do
  let expr = mkApp (mkVar "f") (mkBinApp OpAdd (mkInt 1) (mkInt 2))
  assertFormatsTo expr "f (1 + 2)"
  assertRoundtrips expr

applicationChainFormatting :: Assertion
applicationChainFormatting = do
  let expr = mkApp (mkApp (mkVar "f") (mkVar "x")) (mkVar "y")
  assertFormatsTo expr "f x y"
  assertRoundtrips expr

binaryPrecedence :: Assertion
binaryPrecedence = do
  let expr = mkBinApp OpMul (mkPar (mkBinApp OpAdd (mkInt 1) (mkInt 2))) (mkInt 3)
  assertFormatsTo expr "(1 + 2) * 3"
  assertRoundtrips expr

concatAssociativity :: Assertion
concatAssociativity = do
  let expr = mkBinApp OpConcat (mkVar "a") (mkBinApp OpConcat (mkVar "b") (mkVar "c"))
  assertFormatsTo expr "a ++ b ++ c"
  assertRoundtrips expr

updateAssociativity :: Assertion
updateAssociativity = do
  let expr = mkBinApp OpUpdate (mkVar "a") (mkBinApp OpUpdate (mkVar "b") (mkVar "c"))
  assertFormatsTo expr "a // b // c"
  assertRoundtrips expr

implicationAssociativity :: Assertion
implicationAssociativity = do
  let expr = mkBinApp OpImpl (mkVar "a") (mkBinApp OpImpl (mkVar "b") (mkVar "c"))
  assertFormatsTo expr "a -> b -> c"
  assertRoundtrips expr

equalityParens :: Assertion
equalityParens = do
  let expr = mkBinApp OpEq (mkVar "a") (mkPar (mkBinApp OpEq (mkVar "b") (mkVar "c")))
  assertFormatsTo expr "a == (b == c)"
  assertRoundtrips expr

inheritScopeParens :: Assertion
inheritScopeParens = do
  let expr = mkSet [mkInheritFrom (mkIf (mkVar "cond") (mkVar "pkg") (mkVar "fallback")) ["name"]]
  assertFormatsTo expr "{\n  inherit (if cond then pkg else fallback) name;\n}"
  assertRoundtrips expr

emptyListFormatting :: Assertion
emptyListFormatting = do
  let expr = mkList []
  assertFormatsTo expr "[ ]"
  assertRoundtrips expr

emptySetFormatting :: Assertion
emptySetFormatting = do
  let expr = mkSet []
  assertFormatsTo expr "{ }"
  assertRoundtrips expr

emptyRecSetFormatting :: Assertion
emptyRecSetFormatting = do
  let expr = mkRecSet []
  assertFormatsTo expr "rec { }"
  assertRoundtrips expr

emptyLetFormatting :: Assertion
emptyLetFormatting = do
  let expr = mkLet [] (mkVar "x")
  assertFormatsTo expr "let\nin\nx"
  assertRoundtrips expr

longListFormatting :: Assertion
longListFormatting = do
  let expr = mkList [mkInt 1, mkInt 2, mkInt 3, mkInt 4, mkInt 5]
  assertFormatsTo expr "[\n  1\n  2\n  3\n  4\n  5\n]"
  assertRoundtrips expr

inheritManyFormatting :: Assertion
inheritManyFormatting = do
  let keys = map mkAttr ["a", "b", "c", "d"]
      expr = mkSet [mkInheritKeys keys]
  assertFormatsTo expr "{\n  inherit\n    a\n    b\n    c\n    d\n    ;\n}"
  assertRoundtrips expr

inheritFromManyFormatting :: Assertion
inheritFromManyFormatting = do
  let keys = map mkAttr ["a", "b", "c", "d"]
      expr = mkSet [mkInheritFromKeys (mkVar "pkgs") keys]
  assertFormatsTo expr "{\n  inherit (pkgs)\n    a\n    b\n    c\n    d\n    ;\n}"
  assertRoundtrips expr

nestedSetBindingFormatting :: Assertion
nestedSetBindingFormatting = do
  let expr = mkSet [mkBinding ["x"] (mkSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkInt 2)])]
  assertFormatsTo expr "{\n  x = {\n    a = 1;\n    b = 2;\n  };\n}"
  assertRoundtrips expr

letValueBindingFormatting :: Assertion
letValueBindingFormatting = do
  let expr = mkSet [mkBinding ["x"] (mkLet [mkBinding ["a"] (mkInt 1)] (mkVar "a"))]
  assertFormatsTo expr "{\n  x =\n    let\n      a = 1;\n    in\n    a;\n}"
  assertRoundtrips expr

emptySetPatternFormatting :: Assertion
emptySetPatternFormatting = do
  let expr = mkLam (mkSetPat NixSetPatNotEllipses Nothing []) (mkVar "body")
  assertFormatsTo expr "{ }: body"
  assertRoundtrips expr

ellipsisOnlySetPatternFormatting :: Assertion
ellipsisOnlySetPatternFormatting = do
  let expr = mkLam (mkSetPat NixSetPatIsEllipses Nothing []) (mkVar "body")
  assertFormatsTo expr "{ ... }: body"
  assertRoundtrips expr

simpleSetPatternFormatting :: Assertion
simpleSetPatternFormatting = do
  let pat = mkSetPat NixSetPatNotEllipses Nothing [mkSetPatBinding "a" Nothing, mkSetPatBinding "b" (Just (mkInt 1))]
      expr = mkLam pat (mkVar "body")
  assertFormatsTo expr "{\n  a,\n  b ? 1,\n}:\nbody"
  assertRoundtrips expr

recursiveSetFormatting :: Assertion
recursiveSetFormatting = do
  let expr = mkRecSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkList [mkInt 1, mkInt 2])]
  assertFormatsTo expr "rec {\n  a = 1;\n  b = [\n    1\n    2\n  ];\n}"
  assertRoundtrips expr

letFormatting :: Assertion
letFormatting = do
  let expr = mkLet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkVar "a")] (mkVar "b")
  assertFormatsTo expr "let\n  a = 1;\n  b = a;\nin\nb"
  assertRoundtrips expr

selectDefaultFormatting :: Assertion
selectDefaultFormatting = do
  let expr = mkApp (mkVar "f") (mkSelectOrAttrs (mkVar "cfg") ["a", "b"] (mkIf (mkVar "cond") (mkInt 1) (mkInt 2)))
  assertFormatsTo expr "f (cfg.a.b or (if cond then 1 else 2))"
  assertRoundtrips expr

stringFormatting :: Assertion
stringFormatting = do
  let expr = mkText "a\n${b}\""
  assertFormatsTo expr "\"a\\n\\${b}\\\"\""
  _ <- parseExprOrFail (formatExpr expr)
  pure ()

lambdaSetPatternFormatting :: Assertion
lambdaSetPatternFormatting = do
  let pat = mkSetPat NixSetPatIsEllipses (Just (mkSetPatAs NixSetPatAsLeading "args")) [mkSetPatBinding "x" Nothing, mkSetPatBinding "y" (Just (mkInt 1))]
      expr = mkLam pat (mkVar "x")
  assertFormatsTo expr "args@{\n  x,\n  y ? 1,\n  ...\n}:\nx"
  assertRoundtrips expr

applicationAttrsetFormatting :: Assertion
applicationAttrsetFormatting = do
  let attrset = mkSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkInt 2)]
      expr = mkApp (mkApp (mkVar "f") attrset) (mkVar "x")
  assertFormatsTo expr "f {\n  a = 1;\n  b = 2;\n} x"
  assertRoundtrips expr

applicationLetFormatting :: Assertion
applicationLetFormatting = do
  let letExpr = mkLet [mkBinding ["a"] (mkInt 1)] (mkVar "a")
      expr = mkApp (mkApp (mkVar "f") letExpr) (mkVar "z")
  assertFormatsTo expr "f (\n  let\n    a = 1;\n  in\n  a\n) z"
  assertRoundtrips expr

withFormatting :: Assertion
withFormatting = do
  let expr = mkWith (mkVar "pkgs") (mkApp (mkVar "callPackage") (mkVar "drv"))
  assertFormatsTo expr "with pkgs; callPackage drv"
  assertRoundtrips expr

withAttrsetFormatting :: Assertion
withAttrsetFormatting = do
  let body = mkSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkInt 2)]
      expr = mkWith (mkVar "pkgs") body
  assertFormatsTo expr "with pkgs;\n{\n  a = 1;\n  b = 2;\n}"
  assertRoundtrips expr

assertFormatting :: Assertion
assertFormatting = do
  let expr = mkAssert (mkVar "cond") (mkApp (mkVar "f") (mkVar "x"))
  assertFormatsTo expr "assert cond;\nf x"
  assertRoundtrips expr

assertAttrsetFormatting :: Assertion
assertAttrsetFormatting = do
  let body = mkSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkInt 2)]
      expr = mkAssert (mkVar "cond") body
  assertFormatsTo expr "assert cond;\n{\n  a = 1;\n  b = 2;\n}"
  assertRoundtrips expr

ifFormatting :: Assertion
ifFormatting = do
  let expr = mkIf (mkVar "cond") (mkVar "yes") (mkVar "no")
  assertFormatsTo expr "if cond then yes else no"
  assertRoundtrips expr

elseIfFormatting :: Assertion
elseIfFormatting = do
  let expr = mkIf (mkVar "a") (mkVar "b") (mkIf (mkVar "c") (mkVar "d") (mkVar "e"))
  assertFormatsTo expr "if a then\n  b\nelse if c then\n  d\nelse\n  e"
  assertRoundtrips expr

nixfmtIsNoOp :: Assertion
nixfmtIsNoOp = mapM_ assertNixfmtFixedPoint examples
  where
    examples =
      [ mkInt 1,
        mkList [],
        mkSet [],
        mkRecSet [],
        mkList [mkInt 1, mkInt 2, mkInt 3, mkInt 4, mkInt 5],
        mkApp (mkVar "f") (mkBinApp OpAdd (mkInt 1) (mkInt 2)),
        mkApp (mkApp (mkVar "f") (mkVar "x")) (mkVar "y"),
        mkApp (mkApp (mkVar "f") (mkSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkInt 2)])) (mkVar "x"),
        mkApp (mkApp (mkVar "f") (mkLet [mkBinding ["a"] (mkInt 1)] (mkVar "a"))) (mkVar "z"),
        mkBinApp OpUpdate (mkVar "a") (mkBinApp OpUpdate (mkVar "b") (mkVar "c")),
        mkBinApp OpImpl (mkVar "a") (mkBinApp OpImpl (mkVar "b") (mkVar "c")),
        mkSet [mkInheritKeys (map mkAttr ["a", "b", "c", "d"])],
        mkSet [mkInheritFromKeys (mkVar "pkgs") (map mkAttr ["a", "b", "c", "d"])],
        mkSet [mkBinding ["x"] (mkSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkInt 2)])],
        mkSet [mkBinding ["x"] (mkLet [mkBinding ["a"] (mkInt 1)] (mkVar "a"))],
        mkRecSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkList [mkInt 1, mkInt 2])],
        mkLet [] (mkVar "x"),
        mkLet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkVar "a")] (mkVar "b"),
        mkLam (mkSetPat NixSetPatNotEllipses Nothing []) (mkVar "body"),
        mkLam (mkSetPat NixSetPatIsEllipses Nothing []) (mkVar "body"),
        mkLam (mkSetPat NixSetPatNotEllipses Nothing [mkSetPatBinding "a" Nothing, mkSetPatBinding "b" (Just (mkInt 1))]) (mkVar "body"),
        mkApp (mkVar "f") (mkSelectOrAttrs (mkVar "cfg") ["a", "b"] (mkIf (mkVar "cond") (mkInt 1) (mkInt 2))),
        mkLam (mkSetPat NixSetPatIsEllipses (Just (mkSetPatAs NixSetPatAsLeading "args")) [mkSetPatBinding "x" Nothing, mkSetPatBinding "y" (Just (mkInt 1))]) (mkVar "x"),
        mkSet [mkInheritFrom (mkIf (mkVar "cond") (mkVar "pkg") (mkVar "fallback")) ["name"]],
        mkWith (mkVar "pkgs") (mkApp (mkVar "callPackage") (mkVar "drv")),
        mkWith (mkVar "pkgs") (mkSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkInt 2)]),
        mkAssert (mkVar "cond") (mkApp (mkVar "f") (mkVar "x")),
        mkAssert (mkVar "cond") (mkSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkInt 2)]),
        mkIf (mkVar "a") (mkVar "b") (mkIf (mkVar "c") (mkVar "d") (mkVar "e"))
      ]

collectsMultilineListHint :: Assertion
collectsMultilineListHint = do
  parsed <- parseExprOrFail "[\n  1\n  2\n]"
  let hints = collectLayoutHints parsed
  lookupLayoutHint [] hints @?= Just (LayoutHint (Just PreferMultiline) [] [])

preservesInlineListWithHints :: Assertion
preservesInlineListWithHints = do
  parsed <- parseExprOrFail "[ 1 2 ]"
  let (expr, hints) = fromParsedExpr parsed
  formatExprWithHints hints expr @?= "[ 1 2 ]"

preservesInlineLetWithHints :: Assertion
preservesInlineLetWithHints = do
  parsed <- parseExprOrFail "let a = 1; b = a; in b"
  let (expr, hints) = fromParsedExpr parsed
  formatExprWithHints hints expr @?= "let a = 1; b = a; in b"

preservesInlineInterpolationListWithHints :: Assertion
preservesInlineInterpolationListWithHints = do
  parsed <- parseExprOrFail "\"${[ 1 2 ]}\""
  let (expr, hints) = fromParsedExpr parsed
  formatExprWithHints hints expr @?= "\"${[ 1 2 ]}\""

preservesInlinePathInterpolationListWithHints :: Assertion
preservesInlinePathInterpolationListWithHints = do
  parsed <- parseExprOrFail "./${[ 1 2 ]}"
  let (expr, hints) = fromParsedExpr parsed
  formatExprWithHints hints expr @?= "./${[ 1 2 ]}"

preservesRootLeadingCommentsWithHints :: Assertion
preservesRootLeadingCommentsWithHints = do
  parsed <- parseFileOrFail "<comments>" "# hello\n1"
  let (expr, hints) = fromParsedExpr parsed
  formatExprWithHints hints expr @?= "# hello\n1"

preservesRootTrailingCommentsWithHints :: Assertion
preservesRootTrailingCommentsWithHints = do
  parsed <- parseFileOrFail "<comments>" "1\n# bye"
  let (expr, hints) = fromParsedExpr parsed
  formatExprWithHints hints expr @?= "1\n# bye"

preservesMultipleRootCommentsWithBlankLinesWithHints :: Assertion
preservesMultipleRootCommentsWithBlankLinesWithHints = do
  parsed <- parseFileOrFail "<comments>" "# first\n\n# second\n1"
  let (expr, hints) = fromParsedExpr parsed
  lookupLayoutHint [] hints
    @?= Just
      ( LayoutHint
          Nothing
          [CanonicalLineComment " first", CanonicalBlankLine, CanonicalLineComment " second"]
          []
      )
  formatExprWithHints hints expr @?= "# first\n\n# second\n1"

preservesNestedSetCommentsWithHints :: Assertion
preservesNestedSetCommentsWithHints = do
  parsed <- parseFileOrFail "<comments>" "{\n  # before x\n  x = 1;\n}"
  let (expr, hints) = fromParsedExpr parsed
  lookupLayoutHint [0] hints @?= Nothing
  lookupLayoutHint [0, 0] hints
    @?= Just
      ( LayoutHint
          Nothing
          [CanonicalLineComment " before x"]
          []
      )
  formatExprWithHints hints expr @?= "{\n  # before x\n  x = 1;\n}"

preservesNestedBlankLineCommentsWithHints :: Assertion
preservesNestedBlankLineCommentsWithHints = do
  parsed <- parseFileOrFail "<comments>" "{\n\n  # before x\n  x = 1;\n}"
  let (expr, hints) = fromParsedExpr parsed
  lookupLayoutHint [0] hints @?= Nothing
  lookupLayoutHint [0, 0] hints
    @?= Just
      ( LayoutHint
          Nothing
          [CanonicalBlankLine, CanonicalLineComment " before x"]
          []
      )
  formatExprWithHints hints expr @?= "{\n\n  # before x\n  x = 1;\n}"

preservesSecondBindingBlankLineCommentsWithHints :: Assertion
preservesSecondBindingBlankLineCommentsWithHints = do
  parsed <- parseFileOrFail "<comments>" "{\n  x = 1;\n\n  # before y\n  y = 2;\n}"
  let (expr, hints) = fromParsedExpr parsed
  lookupLayoutHint [1] hints @?= Nothing
  lookupLayoutHint [1, 0] hints
    @?= Just
      ( LayoutHint
          Nothing
          [CanonicalBlankLine, CanonicalLineComment " before y"]
          []
      )
  formatExprWithHints hints expr @?= "{\n  x = 1;\n\n  # before y\n  y = 2;\n}"

preservesInheritBindingCommentsWithHints :: Assertion
preservesInheritBindingCommentsWithHints = do
  parsed <- parseFileOrFail "<comments>" "{\n  # before inherit\n  inherit x;\n}"
  let (expr, hints) = fromParsedExpr parsed
  formatExprWithHints hints expr @?= "{\n  # before inherit\n  inherit x;\n}"

stripParensExpr :: Expr -> Expr
stripParensExpr = \case
  NixVar x name -> NixVar x name
  NixLit x lit -> NixLit x lit
  NixPar _ expr -> stripParensExpr expr
  NixString x str -> NixString x (stripParensString str)
  NixPath x path -> NixPath x (stripParensPath path)
  NixEnvPath x path -> NixEnvPath x path
  NixLam x pat expr -> NixLam x (stripParensFuncPat pat) (stripParensExpr expr)
  NixApp x f y -> NixApp x (stripParensExpr f) (stripParensExpr y)
  NixBinApp x op l r -> NixBinApp x op (stripParensExpr l) (stripParensExpr r)
  NixNotApp x expr -> NixNotApp x (stripParensExpr expr)
  NixNegApp x expr -> NixNegApp x (stripParensExpr expr)
  NixList x xs -> NixList x (stripParensExpr <$> xs)
  NixSet x recFlag bindings -> NixSet x recFlag (stripParensBinding <$> bindings)
  NixLet x bindings expr -> NixLet x (stripParensBinding <$> bindings) (stripParensExpr expr)
  NixHasAttr x expr path -> NixHasAttr x (stripParensExpr expr) (stripParensAttrPath path)
  NixSelect x expr path mDefault -> NixSelect x (stripParensExpr expr) (stripParensAttrPath path) (stripParensExpr <$> mDefault)
  NixIf x c t f -> NixIf x (stripParensExpr c) (stripParensExpr t) (stripParensExpr f)
  NixWith x scope expr -> NixWith x (stripParensExpr scope) (stripParensExpr expr)
  NixAssert x assertion expr -> NixAssert x (stripParensExpr assertion) (stripParensExpr expr)

stripParensString :: NString -> NString
stripParensString = \case
  NixDoubleQuotesString x parts -> NixDoubleQuotesString x (stripParensStringPart <$> parts)
  NixDoubleSingleQuotesString x parts -> NixDoubleSingleQuotesString x (stripParensStringPart <$> parts)

stripParensPath :: Path -> Path
stripParensPath = \case
  NixLiteralPath x path -> NixLiteralPath x path
  NixInterpolPath x parts -> NixInterpolPath x (stripParensStringPart <$> parts)

stripParensStringPart :: StringPart -> StringPart
stripParensStringPart = \case
  NixStringLiteral x text -> NixStringLiteral x text
  NixStringInterpol x expr -> NixStringInterpol x (stripParensExpr expr)

stripParensAttrPath :: AttrPath -> AttrPath
stripParensAttrPath (NixAttrPath x keys) = NixAttrPath x (stripParensAttrKey <$> keys)

stripParensAttrKey :: AttrKey -> AttrKey
stripParensAttrKey = \case
  NixStaticAttrKey x name -> NixStaticAttrKey x name
  NixDynamicStringAttrKey x parts -> NixDynamicStringAttrKey x (stripParensStringPart <$> parts)
  NixDynamicInterpolAttrKey x expr -> NixDynamicInterpolAttrKey x (stripParensExpr expr)

stripParensBinding :: Binding -> Binding
stripParensBinding = \case
  NixNormalBinding x path expr -> NixNormalBinding x (stripParensAttrPath path) (stripParensExpr expr)
  NixInheritBinding x mScope keys -> NixInheritBinding x (stripParensExpr <$> mScope) (stripParensAttrKey <$> keys)

stripParensFuncPat :: FuncPat -> FuncPat
stripParensFuncPat = \case
  NixVarPat x name -> NixVarPat x name
  NixSetPat x ellipses mAs bindings -> NixSetPat x ellipses mAs (stripParensSetPatBinding <$> bindings)

stripParensSetPatBinding :: SetPatBinding -> SetPatBinding
stripParensSetPatBinding NixSetPatBinding {..} = NixSetPatBinding nspbAnn nspbVar (stripParensExpr <$> nspbDefault)
