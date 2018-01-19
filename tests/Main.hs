module Main (main) where

import qualified Language.SimpleBool.QuoteTests as S
import qualified Language.Untyped.QuoteTests    as U
import           Test.Tasty


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ U.quoteTests
  , S.quoteTests
  ]

main :: IO ()
main = defaultMain unitTests
