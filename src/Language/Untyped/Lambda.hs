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
  deriving (Eq, Data, Typeable)

printTm :: Term -> String
printTm (TmVar _ v)                                 = v
printTm (TmAbs _ v t)                               = "\\" ++ v ++ ". " ++ printTm t
printTm (TmApp _ t1@(TmVar _ _)   t2@(TmAbs _ _ _)) =        printTm t1 ++        " (" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmVar _ _)   t2@(TmApp _ _ _)) =        printTm t1 ++        " (" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmApp _ _ _) t2@(TmAbs _ _ _)) =        printTm t1 ++        " (" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmApp _ _ _) t2@(TmApp _ _ _)) =        printTm t1 ++        " (" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmAbs _ _ _) t2@(TmAbs _ _ _)) = "(" ++ printTm t1 ++ ") " ++ "(" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmAbs _ _ _) t2@(TmApp _ _ _)) = "(" ++ printTm t1 ++ ") " ++ "(" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmAbs _ _ _) t2)               = "(" ++ printTm t1 ++ ") "        ++ printTm t2
printTm (TmApp _ t1               t2)               =        printTm t1 ++ " "         ++ printTm t2

instance Show Term where
  show = printTm
