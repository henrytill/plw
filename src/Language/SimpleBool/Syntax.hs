{-# LANGUAGE DeriveDataTypeable #-}

module Language.SimpleBool.Syntax
  ( Ty (..)
  , Binding (..)
  , addBinding
  , getBinding
  , getTypeFromContext
  , TermN (..)
  , TermB (..)
  , termNtoB
  ) where

import           Data.Data
import           Data.List     (elemIndex)
import           Language.Base (Info)


data Ty
  = TyBool
  | TyArr Ty Ty
  deriving (Eq, Data, Typeable)

instance Show Ty where
  show TyBool      = "Bool"
  show (TyArr d c) = show d ++ " -> " ++ show c

data Binding
  = NameBind
  | VarBind Ty
  deriving (Eq, Show, Data, Typeable)

type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

getBinding :: Info -> Context -> Int -> Either String Binding
getBinding _ ctx i
  = maybe l Right (snd <$> lookup i (zip [0..] ctx))
  where
    l = Left ("Variable lookup failure: offset: " ++ show i ++ ", ctx size: " ++ show (length ctx))

getTypeFromContext :: Info -> Context -> Int -> Either String Ty
getTypeFromContext fi ctx i =
   case getBinding fi ctx i of
     Right (VarBind tyT) -> Right tyT
     Right _             -> Left "getTypeFromContext: Wrong kind of binding for variable"
     Left msg            -> Left msg

-- * String-based names

data TermN
  = TmTrueN Info
  | TmFalseN Info
  | TmIfN Info TermN TermN TermN
  | TmVarN Info String
  | TmAbsN Info String Ty TermN
  | TmAppN Info TermN TermN
  | TmMetaVarN Info String
  deriving (Data, Typeable)

printTmN :: TermN -> String
printTmN (TmTrueN _)                                        = "true"
printTmN (TmFalseN _)                                       = "false"
printTmN (TmIfN _ p c a)                                    = "if " ++ printTmN p ++ " then " ++ printTmN c ++ " else " ++ printTmN a
printTmN (TmVarN _ s)                                       = s
printTmN (TmAbsN _ _ ty t)                                  = "\\ " ++ show ty ++ ". " ++ printTmN t
printTmN (TmAppN _ t1@(TmVarN _ _)     t2@(TmAbsN _ _ _ _)) =        printTmN t1 ++        " (" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmVarN _ _)     t2@(TmAppN _ _ _))   =        printTmN t1 ++        " (" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAppN _ _ _)   t2@(TmAbsN _ _ _ _)) =        printTmN t1 ++        " (" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAppN _ _ _)   t2@(TmAppN _ _ _))   =        printTmN t1 ++        " (" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAbsN _ _ _ _) t2@(TmAbsN _ _ _ _)) = "(" ++ printTmN t1 ++ ") " ++ "(" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAbsN _ _ _ _) t2@(TmAppN _ _ _))   = "(" ++ printTmN t1 ++ ") " ++ "(" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAbsN _ _ _ _) t2)                  = "(" ++ printTmN t1 ++ ") "        ++ printTmN t2
printTmN (TmAppN _ t1                  t2)                  =        printTmN t1 ++ " "         ++ printTmN t2
printTmN (TmMetaVarN _ _)                                   = error "Attempting to print a metavariable"

instance Show TermN where
  show = printTmN

-- * de-Bruijn indices

data TermB
  = TmTrueB Info
  | TmFalseB Info
  | TmIfB Info TermB TermB TermB
  | TmVarB Info Int Int
  | TmAbsB Info String Ty TermB
  | TmAppB Info TermB TermB
  deriving (Data, Typeable)

printTmB :: TermB -> String
printTmB (TmTrueB _)                                        = "true"
printTmB (TmFalseB _)                                       = "false"
printTmB (TmIfB _ p c a)                                    = "if " ++ printTmB p ++ " then " ++ printTmB c ++ " else " ++ printTmB a
printTmB (TmVarB _ i _)                                     = show i
printTmB (TmAbsB _ _ ty t)                                  = "\\ " ++ show ty ++ ". " ++ printTmB t
printTmB (TmAppB _ t1@(TmVarB _ _ _)   t2@(TmAbsB _ _ _ _)) =        printTmB t1 ++        " (" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmVarB _ _ _)   t2@(TmAppB _ _ _))   =        printTmB t1 ++        " (" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAppB _ _ _)   t2@(TmAbsB _ _ _ _)) =        printTmB t1 ++        " (" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAppB _ _ _)   t2@(TmAppB _ _ _))   =        printTmB t1 ++        " (" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAbsB _ _ _ _) t2@(TmAbsB _ _ _ _)) = "(" ++ printTmB t1 ++ ") " ++ "(" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAbsB _ _ _ _) t2@(TmAppB _ _ _))   = "(" ++ printTmB t1 ++ ") " ++ "(" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAbsB _ _ _ _) t2)                  = "(" ++ printTmB t1 ++ ") "        ++ printTmB t2
printTmB (TmAppB _ t1                  t2)                  =        printTmB t1 ++ " "         ++ printTmB t2

instance Show TermB where
  show = printTmB

instance Eq TermB where
  TmVarB _ i1 t1     == TmVarB _ i2 t2     = i1 == i2 && t1  == t2
  TmAbsB _ s1 ty1 t1 == TmAbsB _ s2 ty2 t2 = s1 == s2 && ty1 == ty2 && t1 == t2
  TmAppB _ l1 l2     == TmAppB _ r1 r2     = l1 == r1 && l2  == r2
  _                  == _                  = False

termNtoB :: TermN -> TermB
termNtoB = to []
  where
    to :: [String] -> TermN -> TermB
    to _  (TmTrueN info)       = TmTrueB info
    to _  (TmFalseN info)      = TmFalseB info
    to vs (TmIfN info p c a)   = TmIfB info (to vs p) (to vs c) (to vs a)
    to vs (TmVarN info v)      = maybe (TmVarB info 0 (length vs))
                                       (\ x -> TmVarB info x (length vs))
                                       (elemIndex v vs)
    to vs (TmAbsN info x ty b) = TmAbsB info x ty (to (x : vs) b)
    to vs (TmAppN info f a)    = TmAppB info (to vs f) (to vs a)
    to _  (TmMetaVarN _ _)     = error "Attempting to convert a metavariable to its de-Bruijn-indexed version"
