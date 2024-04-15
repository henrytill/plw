{-# LANGUAGE QuasiQuotes #-}

module Language.Calc.QuoteTests where

import Language.Calc.Quote (intro)
import Language.Calc.Syntax
import Test.Tasty
import Test.Tasty.HUnit

parseVar :: TestTree
parseVar =
  testCase "a" $
    assertEqual
      "For the result of parse, "
      (Var "a")
      [intro| a |]

parseConst :: TestTree
parseConst =
  testCase "42" $
    assertEqual
      "For the result of parse, "
      (Const 42)
      [intro| 42 |]

parseAdd :: TestTree
parseAdd =
  testCase "42 + 42" $
    assertEqual
      "For the result of parse, "
      (Add (Const 42) (Const 42))
      [intro| 42 + 42 |]

parseMul :: TestTree
parseMul =
  testCase "42 * 42" $
    assertEqual
      "For the result of parse, "
      (Mul (Const 42) (Const 42))
      [intro| 42 * 42 |]

parseExp :: TestTree
parseExp =
  testCase "2 ^ 3" $
    assertEqual
      "For the result of parse, "
      (Exp (Const 2) (Const 3))
      [intro| 2 ^ 3 |]

parseSubNeg :: TestTree
parseSubNeg =
  testCase "x - - - x" $
    assertEqual
      "For the result of parse, "
      (Sub (Var "x") (Neg (Neg (Var "x"))))
      [intro| x - - - x |]

parseMulAdd :: TestTree
parseMulAdd =
  testCase "x * y + z" $
    assertEqual
      "For the result of parse, "
      (Add (Mul (Var "x") (Var "y")) (Var "z"))
      [intro| x * y + z |]

parseExample :: TestTree
parseExample =
  testCase "(0 * x + 1) * 3 + 12" $
    assertEqual
      "For the result of parse, "
      (Add (Mul (Add (Mul (Const 0) (Var "x")) (Const 1)) (Const 3)) (Const 12))
      [intro| (0 * x + 1) * 3 + 12 |]

parseMeta :: TestTree
parseMeta =
  testCase "$m - 1" $
    assertEqual
      "For the result of parse, "
      (Sub (Const 1) (Const 1))
      [intro| $m - 1 |]
  where
    m = Const 1

quoteTests :: TestTree
quoteTests =
  testGroup
    "Intro"
    [ parseVar,
      parseConst,
      parseAdd,
      parseMul,
      parseExp,
      parseSubNeg,
      parseMulAdd,
      parseExample,
      parseMeta
    ]
