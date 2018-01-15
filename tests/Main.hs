module Main (main) where

import           Test.Tasty
import           Language.Untyped.QuoteTests


unitTests :: TestTree
unitTests = testGroup "Unit tests" quoteTests

main :: IO ()
main = defaultMain unitTests
