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
import Utils (parseExprOrFail)

tests :: TestTree
tests =
  testGroup
    "rfc print"
    [ testCase "formats application argument with required parentheses" applicationArgumentParens,
      testCase "formats binary precedence correctly" binaryPrecedence,
      testCase "formats right associative concat" concatAssociativity,
      testCase "formats non associative equality with parentheses" equalityParens,
      testCase "formats inherit scope with parentheses" inheritScopeParens,
      testCase "formats set recursively" recursiveSetFormatting,
      testCase "formats let expression multiline" letFormatting,
      testCase "formats select default without losing precedence" selectDefaultFormatting,
      testCase "formats strings and reparses" stringFormatting,
      testCase "formats lambda set pattern" lambdaSetPatternFormatting,
      testCase "nixfmt is no-op for fresh Syn output" nixfmtIsNoOp,
      testCase "collects multiline list hint from parsed source" collectsMultilineListHint,
      testCase "parsed source can preserve inline list layout with hints" preservesInlineListWithHints,
      testCase "parsed source can preserve inline let layout with hints" preservesInlineLetWithHints,
      testCase "parsed source can preserve inline interpolation list layout with hints" preservesInlineInterpolationListWithHints,
      testCase "parsed source can preserve inline path interpolation list layout with hints" preservesInlinePathInterpolationListWithHints
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

nixfmtIsNoOp :: Assertion
nixfmtIsNoOp = mapM_ assertNixfmtFixedPoint examples
  where
    examples =
      [ mkInt 1,
        mkApp (mkVar "f") (mkBinApp OpAdd (mkInt 1) (mkInt 2)),
        mkRecSet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkList [mkInt 1, mkInt 2])],
        mkLet [mkBinding ["a"] (mkInt 1), mkBinding ["b"] (mkVar "a")] (mkVar "b"),
        mkApp (mkVar "f") (mkSelectOrAttrs (mkVar "cfg") ["a", "b"] (mkIf (mkVar "cond") (mkInt 1) (mkInt 2))),
        mkLam (mkSetPat NixSetPatIsEllipses (Just (mkSetPatAs NixSetPatAsLeading "args")) [mkSetPatBinding "x" Nothing, mkSetPatBinding "y" (Just (mkInt 1))]) (mkVar "x"),
        mkSet [mkInheritFrom (mkIf (mkVar "cond") (mkVar "pkg") (mkVar "fallback")) ["name"]]
      ]

collectsMultilineListHint :: Assertion
collectsMultilineListHint = do
  parsed <- parseExprOrFail "[\n  1\n  2\n]"
  let hints = collectLayoutHints parsed
  lookupLayoutHint [] hints @?= Just (LayoutHint (Just PreferMultiline))

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
