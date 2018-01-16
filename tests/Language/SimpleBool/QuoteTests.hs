{-# LANGUAGE QuasiQuotes              #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.SimpleBool.QuoteTests where

import           Language.SimpleBool.Syntax (Term)
import           Language.SimpleBool.Quote  (simpleBool)
import           Test.Tasty

identity :: Term
identity = [simpleBool| \x : Bool. x |]

ifExpr01 :: Term
ifExpr01 = [simpleBool| \x : Bool -> Bool. if x false then true else false |]

ifExpr02 :: Term
ifExpr02 = [simpleBool| \x : Bool -> Bool. if x then false else true |]

appIf :: Term
appIf = [simpleBool| $ifExpr01 $ifExpr02 |]

quoteTests :: [TestTree]
quoteTests = []
