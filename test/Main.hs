module Main (main) where

import Language.SimpleBool.QuoteTests qualified as SimpleBool
import Language.Untyped.QuoteTests qualified as Untyped
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ Untyped.quoteTests
    , SimpleBool.quoteTests
    ]

main :: IO ()
main = defaultMain unitTests
