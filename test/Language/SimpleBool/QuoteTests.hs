{-# LANGUAGE QuasiQuotes #-}

module Language.SimpleBool.QuoteTests where

import Language.SimpleBool.Core
import Language.SimpleBool.Quote (simpleBool)
import Language.SimpleBool.Syntax
import Test.Tasty
import Test.Tasty.HUnit

makeTest :: TermN -> Either String Ty -> Either String Ty -> TestTree
makeTest term expected actual = testCase description $ assertEqual forTheResult expected actual
  where
    description = "typeOf " ++ show term
    forTheResult = "For the result of typeOf, "

identity :: TermN
identity = [simpleBool| \x : Bool. x |]

testTypeOfIdentity :: TestTree
testTypeOfIdentity = makeTest identity expected actual
  where
    expected = Right (TyArr TyBool TyBool)
    actual = typeOf [] (termNtoB identity)

ifExpr01 :: TermN
ifExpr01 = [simpleBool| \x : Bool -> Bool. if x false then true else false |]

testTypeOfIfExpr01 :: TestTree
testTypeOfIfExpr01 = makeTest ifExpr01 expected actual
  where
    expected = Right (TyArr (TyArr TyBool TyBool) TyBool)
    actual = typeOf [] (termNtoB ifExpr01)

ifExpr02 :: TermN
ifExpr02 = [simpleBool| \x : Bool. if x then false else true |]

testTypeOfIfExpr02 :: TestTree
testTypeOfIfExpr02 = makeTest ifExpr02 expected actual
  where
    expected = Right (TyArr TyBool TyBool)
    actual = typeOf [] (termNtoB ifExpr02)

appIf :: TermN
appIf = [simpleBool| $ifExpr01 $ifExpr02 |]

testTypeOfAppIf :: TestTree
testTypeOfAppIf = makeTest appIf expected actual
  where
    expected = Right TyBool
    actual = typeOf [] (termNtoB appIf)

quoteTests :: TestTree
quoteTests =
  testGroup
    "Simply-typed Lambda Calculus"
    [ testTypeOfIdentity
    , testTypeOfIfExpr01
    , testTypeOfIfExpr02
    , testTypeOfAppIf
    ]
