{-# LANGUAGE DeriveDataTypeable #-}

module Language.SimpleBool.Syntax where

import           Data.Data
import           Language.Base (Info)

data Ty
  = TyBool
  | TyArr Ty Ty
  deriving (Eq, Show, Data, Typeable)

data Term
  = TmTrue Info
  | TmFalse Info
  | TmIf Info Term Term Term
  | TmVar Info String
  | TmAbs Info String Ty Term
  | TmApp Info Term Term
  | TmMetaVar Info String
  deriving (Eq, Show, Data, Typeable)

data Binding
  = NameBind
  | VarBind Ty
  deriving (Eq, Show, Data, Typeable)

type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx
