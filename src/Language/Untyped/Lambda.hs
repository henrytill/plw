{-# LANGUAGE DeriveDataTypeable #-}

module Language.Untyped.Lambda (Term(..)) where

import           Data.Data
import           Data.Typeable
import           Language.Untyped.Base (Info)


data Term
  = TmVar Info String
  | TmAbs Info String Term
  | TmApp Info Term Term
  | TmMetaVar Info String
  deriving (Eq, Show, Data, Typeable)

-- instance Show Term where
--   show (TmVar _ v)                   = v
--   show (TmAbs _ v t)                 = "\\" ++ v ++ "." ++ show t
--   show (TmApp _ t1@(TmAbs _ _ _) t2) = "(" ++ show t1 ++ ") " ++ show t2
--   show (TmApp _ t1               t2) =        show t1 ++ " "  ++ show t2
