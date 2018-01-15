module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.Untyped.QuoteTests


unitTests = testGroup "Unit tests" quoteTests

main :: IO ()
main = defaultMain unitTests
