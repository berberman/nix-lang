module Main (main) where

import qualified Edit
import qualified ExactPrint
import qualified Parser
import qualified RFCPrint
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "nix-lang"
    [ Parser.tests,
      ExactPrint.tests,
      RFCPrint.tests,
      Edit.tests
    ]
