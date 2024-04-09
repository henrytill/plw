{-# LANGUAGE DeriveDataTypeable #-}

module Language.Calc.Syntax where

import Data.Data

data Expression
  = Var String
  | Const Integer
  | Neg Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Exp Expression Expression
  | MetaVar String
  deriving (Show, Eq, Data, Typeable)
