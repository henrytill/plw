module Main (main) where

import Language.Intro.QuoteTests qualified as I
import Language.SimpleBool.QuoteTests qualified as S
import Language.Untyped.QuoteTests qualified as U
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ I.quoteTests,
      U.quoteTests,
      S.quoteTests
    ]

main :: IO ()
main = defaultMain unitTests
