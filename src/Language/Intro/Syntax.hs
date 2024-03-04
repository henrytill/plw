{-# LANGUAGE DeriveDataTypeable #-}

module Language.Intro.Syntax where

import Data.Data

data Expression
  = Var String
  | Const Integer
  | Add Expression Expression
  | Mul Expression Expression
  | MetaVar String
  deriving (Show, Eq, Data, Typeable)
