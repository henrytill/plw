module Main (main) where

import qualified Language.SimpleBool.QuoteTests as QT
import qualified Language.Untyped.QuoteTests    as UT
import           Test.Tasty


unitTests :: TestTree
unitTests = testGroup "Unit tests" (UT.quoteTests ++ QT.quoteTests)

main :: IO ()
main = defaultMain unitTests
