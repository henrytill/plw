{-# LANGUAGE QuasiQuotes              #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.SimpleBool.QuoteTests where

import           Language.SimpleBool.Syntax (TermN)
import           Language.SimpleBool.Quote  (simpleBool)
import           Test.Tasty


identity :: TermN
identity = [simpleBool| \x : Bool. x |]

ifExpr01 :: TermN
ifExpr01 = [simpleBool| \x : Bool -> Bool. if x false then true else false |]

ifExpr02 :: TermN
ifExpr02 = [simpleBool| \x : Bool -> Bool. if x then false else true |]

appIf :: TermN
appIf = [simpleBool| $ifExpr01 $ifExpr02 |]

quoteTests :: [TestTree]
quoteTests = []
