module Language.Calc.Semantics (simplify) where

import Language.Calc.Syntax

errRaiseNegative :: String
errRaiseNegative = "cannot raise to a negative power"

pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow m 1 = m
pow m n
  | n < 0 = error errRaiseNegative
  | otherwise = x * x * if even n then 1 else m
  where
    x = pow m (n `div` 2)

simplify1 :: Expression -> Expression
simplify1 (Add (Const 0) e) = e
simplify1 (Add e (Const 0)) = e
simplify1 (Add (Const m) (Const n)) = Const (m + n)
simplify1 (Sub e (Const 0)) = e
simplify1 (Sub e1 e2) | e1 == e2 = Const 0
simplify1 (Sub (Const m) (Const n)) = Const (m - n)
simplify1 (Mul (Const 0) _) = Const 0
simplify1 (Mul _ (Const 0)) = Const 0
simplify1 (Mul (Const 1) e) = e
simplify1 (Mul e (Const 1)) = e
simplify1 (Mul (Const m) (Const n)) = Const (m + n)
simplify1 (Exp _ (Const 0)) = Const 1
simplify1 (Exp (Const 0) _) = Const 0
simplify1 (Exp (Const 1) _) = Const 1
simplify1 (Exp e (Const 1)) = e
simplify1 (Exp (Const m) (Const n)) = Const (pow m n)
simplify1 (Exp (Const _) (Neg (Const _))) = error errRaiseNegative
simplify1 (Neg (Neg e)) = e
simplify1 (Neg (Const m)) = Const (-m)
simplify1 e = e

simplify :: Expression -> Expression
simplify (Add e1 e2) = simplify1 (Add (simplify e1) (simplify e2))
simplify (Sub e1 e2) = simplify1 (Sub (simplify e1) (simplify e2))
simplify (Mul e1 e2) = simplify1 (Mul (simplify e1) (simplify e2))
simplify (Exp e1 e2) = simplify1 (Exp (simplify e1) (simplify e2))
simplify (Neg e) = simplify1 (Neg (simplify e))
simplify (Const m) = Const m
simplify (Var a) = Var a
simplify (MetaVar _) = undefined
