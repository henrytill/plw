module Main (main) where

import Language.Calc.QuoteTests qualified as Calc
import Language.Intro.QuoteTests qualified as Intro
import Language.SimpleBool.QuoteTests qualified as SimpleBool
import Language.Untyped.QuoteTests qualified as Untyped
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ Calc.quoteTests,
      Intro.quoteTests,
      Untyped.quoteTests,
      SimpleBool.quoteTests
    ]

main :: IO ()
main = defaultMain unitTests
