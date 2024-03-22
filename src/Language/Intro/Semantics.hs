module Language.Intro.Semantics (simplify) where

import Language.Intro.Syntax

errRaiseNegative :: String
errRaiseNegative = "cannot raise to a negative power"

pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow a 1 = a
pow a n
  | n < 0 = error errRaiseNegative
  | otherwise = b * b * if even n then 1 else a
  where
    b = pow a (n `div` 2)

simplify1 :: Expression -> Expression
simplify1 (Add (Const 0) x) = x
simplify1 (Add x (Const 0)) = x
simplify1 (Add (Const m) (Const n)) = Const (m + n)
simplify1 (Sub x (Const 0)) = x
simplify1 (Sub x y) | x == y = Const 0
simplify1 (Sub (Const m) (Const n)) = Const (m - n)
simplify1 (Mul (Const 0) _) = Const 0
simplify1 (Mul _ (Const 0)) = Const 0
simplify1 (Mul (Const 1) x) = x
simplify1 (Mul x (Const 1)) = x
simplify1 (Mul (Const m) (Const n)) = Const (m + n)
simplify1 (Exp _ (Const 0)) = Const 1
simplify1 (Exp (Const 0) _) = Const 0
simplify1 (Exp (Const 1) _) = Const 1
simplify1 (Exp x (Const 1)) = x
simplify1 (Exp (Const m) (Const n)) = Const (pow m n)
simplify1 (Exp (Const _) (Neg (Const _))) = error errRaiseNegative
simplify1 (Neg (Neg m)) = m
simplify1 (Neg (Const m)) = Const (-m)
simplify1 expr = expr

simplify :: Expression -> Expression
simplify (Add e1 e2) = simplify1 (Add (simplify e1) (simplify e2))
simplify (Sub e1 e2) = simplify1 (Sub (simplify e1) (simplify e2))
simplify (Mul e1 e2) = simplify1 (Mul (simplify e1) (simplify e2))
simplify (Exp e1 e2) = simplify1 (Exp (simplify e1) (simplify e2))
simplify expr = simplify1 expr
