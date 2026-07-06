{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad (forM_, foldM)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Nix.Lang.Outputable (renderToText)
import Nix.Lang.Parser (nixFile, runNixParser)
import Nix.Lang.RFCPrint (formatExpr)
import Nix.Lang.RFCPrint.LayoutHints (lowerParsedExpr)
import qualified Nix.Lang.Types.Ps as Parsed
import qualified Nix.Lang.Types.Syn as Syn
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.Mem (performGC)
import System.Process (readProcessWithExitCode)
import Text.Megaparsec (errorBundlePretty)

data Workload = Workload
  { workloadTarget :: String,
    workloadGenerated :: T.Text,
    workloadParsed :: Parsed.Expr,
    workloadLowered :: Syn.Expr
  }

data BenchConfig = BenchConfig
  { bcBuildLikeOuter :: Int,
    bcBuildLikeInner :: Int,
    bcRenderOuter :: Int,
    bcRenderInner :: Int,
    bcReparseOuter :: Int,
    bcReparseInner :: Int
  }

main :: IO ()
main = do
  args <- getArgs
  let targets = if null args then ["."] else args
  putStrLn "# cabal2nix backend benchmark"
  putStrLn "# comparing real cabal2nix output with nix-lang parse/lower/render paths"
  putStrLn ""
  workloads <- mapM prepareWorkload targets
  forM_ workloads $ \workload -> runBenchmarks (configForWorkload workload) workload

prepareWorkload :: String -> IO Workload
prepareWorkload target = do
  putStrLn $ "Preparing workload: " <> target
  generated <- loadWorkloadText target
  let parsed =
        case runNixParser nixFile target generated of
          (Right expr, _) -> expr
          (Left err, _) -> error (errorBundlePretty err)
      lowered = lowerParsedExpr parsed
  T.length generated `seq` pure ()
  putStrLn $ "  generated chars: " <> show (T.length generated)
  putStrLn ""
  pure Workload {workloadTarget = target, workloadGenerated = generated, workloadParsed = parsed, workloadLowered = lowered}

configForWorkload :: Workload -> BenchConfig
configForWorkload workload
  | chars >= 10000000 = BenchConfig 3 1 3 2 2 1
  | chars >= 1000000 = BenchConfig 5 2 5 20 3 2
  | chars >= 100000 = BenchConfig 10 10 10 100 5 10
  | otherwise = BenchConfig 20 200 20 2000 20 100
  where
    chars = T.length (workloadGenerated workload)

runBenchmarks :: BenchConfig -> Workload -> IO ()
runBenchmarks config workload = do
  putStrLn $ "== workload: " <> workloadTarget workload
  sourceLoad <- benchmarkIO 3 $ \_ -> do
    txt <- loadWorkloadText (workloadTarget workload)
    evaluate (T.length txt)
  printMetric "source load" 3 sourceLoad

  buildLike <- benchmarkIO (bcBuildLikeOuter config) $ \outer -> do
    total <- foldM (\acc inner -> (+ acc) <$> buildLikeOnce outer inner workload) 0 [1 .. bcBuildLikeInner config]
    evaluate total
  printMetricBatched "fresh-tree reconstruction + RFCPrint" (bcBuildLikeOuter config) (bcBuildLikeInner config) buildLike

  rfcPrint <- benchmarkIO (bcRenderOuter config) $ \outer -> do
    total <- foldM (\acc inner -> (+ acc) <$> rfcPrintOnce outer inner workload) 0 [1 .. bcRenderInner config]
    evaluate total
  printMetricBatched "RFCPrint formatExpr" (bcRenderOuter config) (bcRenderInner config) rfcPrint

  outputable <- benchmarkIO (bcRenderOuter config) $ \outer -> do
    total <- foldM (\acc inner -> (+ acc) <$> outputableOnce outer inner workload) 0 [1 .. bcRenderInner config]
    evaluate total
  printMetricBatched "Outputable renderToText" (bcRenderOuter config) (bcRenderInner config) outputable

  reparsed <- benchmarkIO (bcReparseOuter config) $ \outer -> do
    total <- foldM (\acc inner -> (+ acc) <$> reparseOnce outer inner workload) 0 [1 .. bcReparseInner config]
    evaluate total
  printMetricBatched "RFCPrint render + reparse" (bcReparseOuter config) (bcReparseInner config) reparsed

  let canonical = formatExpr (workloadLowered workload)
  putStrLn $ "canonical chars: " <> show (T.length canonical)
  putStrLn "sample output preview:"
  T.putStrLn (T.unlines (take 8 (T.lines canonical)))
  putStrLn ""

benchmarkIO :: Int -> (Int -> IO a) -> IO NominalDiffTime
benchmarkIO iterations action = do
  warmup (action 0)
  performGC
  start <- getCurrentTime
  _ <- mapM action [1 .. iterations]
  end <- getCurrentTime
  pure (diffUTCTime end start)

warmup :: IO a -> IO ()
warmup action = do
  _ <- action
  pure ()

buildLikeOnce :: Int -> Int -> Workload -> IO Int
buildLikeOnce outer inner workload = do
  let canonical = rerenderRFC outer inner (workloadLowered workload)
      rebuilt =
        case runNixParser nixFile "<fresh-tree-bench>" canonical of
          (Right expr, _) -> lowerParsedExpr expr
          (Left err, _) -> error (errorBundlePretty err)
      txt = formatExpr rebuilt
  forceText txt

rfcPrintOnce :: Int -> Int -> Workload -> IO Int
rfcPrintOnce outer inner workload = do
  let txt = rerenderRFC outer inner (workloadLowered workload)
  forceText txt

outputableOnce :: Int -> Int -> Workload -> IO Int
outputableOnce outer inner workload = do
  let txt = rerenderOutputable outer inner (workloadLowered workload)
  forceText txt

reparseOnce :: Int -> Int -> Workload -> IO Int
reparseOnce outer inner workload = do
  let rendered = rerenderRFC outer inner (workloadLowered workload)
  case runNixParser nixFile "<rfc-bench>" rendered of
    (Right expr, _) -> evaluate (length (show expr))
    (Left err, _) -> error (errorBundlePretty err)

forceText :: T.Text -> IO Int
forceText txt = do
  let codepointSum = T.foldl' (\acc ch -> acc + fromEnum ch) 0 txt
  evaluate codepointSum

rerenderRFC :: Int -> Int -> Syn.Expr -> T.Text
rerenderRFC outer inner expr = outer `seq` inner `seq` formatExpr expr
{-# NOINLINE rerenderRFC #-}

rerenderOutputable :: Int -> Int -> Syn.Expr -> T.Text
rerenderOutputable outer inner expr = outer `seq` inner `seq` renderToText expr
{-# NOINLINE rerenderOutputable #-}

printMetric :: String -> Int -> NominalDiffTime -> IO ()
printMetric label iterations total = do
  let meanSeconds = realToFrac total / fromIntegral iterations :: Double
      totalSeconds = realToFrac total :: Double
  putStrLn $ label <> ": total=" <> show totalSeconds <> "s mean=" <> show meanSeconds <> "s"

printMetricBatched :: String -> Int -> Int -> NominalDiffTime -> IO ()
printMetricBatched label outer inner total = do
  let totalOps = outer * inner
      totalSeconds = realToFrac total :: Double
      meanOuter = totalSeconds / fromIntegral outer
      meanOp = totalSeconds / fromIntegral totalOps
  putStrLn $ label <> ": total=" <> show totalSeconds <> "s mean/sample=" <> show meanOuter <> "s mean/op=" <> show meanOp <> "s"

captureCabal2nix :: String -> IO T.Text
captureCabal2nix target = do
  let command = "nix"
      args = ["run", "--no-write-lock-file", "nixpkgs#cabal2nix", "--", target]
  (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""
  case exitCode of
    ExitSuccess -> pure (T.pack stdout)
    ExitFailure code -> do
      putStrLn stderr
      putStrLn $ "cabal2nix command failed for target: " <> target
      putStrLn $ "exit code: " <> show code
      exitFailure

loadWorkloadText :: String -> IO T.Text
loadWorkloadText target = do
  isFile <- doesFileExist target
  if isFile || ".nix" `isSuffixOf` target
    then T.readFile target
    else captureCabal2nix target
