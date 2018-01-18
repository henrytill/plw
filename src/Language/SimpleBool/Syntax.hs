{-# LANGUAGE DeriveDataTypeable #-}

module Language.SimpleBool.Syntax where

import           Data.Data
import           Language.Base (Info)


data Ty
  = TyBool
  | TyArr Ty Ty
  deriving (Eq, Show, Data, Typeable)

data TermN
  = TmTrueN Info
  | TmFalseN Info
  | TmIfN Info TermN TermN TermN
  | TmVarN Info String
  | TmAbsN Info String Ty TermN
  | TmAppN Info TermN TermN
  | TmMetaVarN Info String
  deriving (Eq, Show, Data, Typeable)

data Binding
  = NameBind
  | VarBind Ty
  deriving (Eq, Show, Data, Typeable)

type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx
