{-# LANGUAGE DeriveDataTypeable #-}

module Language.SimpleBool.Syntax
  ( Ty (..)
  , Binding (..)
  , Context
  , addBinding
  , getBinding
  , getTypeFromContext
  , TermN (..)
  , TermB (..)
  , termNtoB
  )
where

import Data.Data
import Data.List (elemIndex)
import Language.Base (Info)

data Ty
  = TyBool
  | TyArr Ty Ty
  deriving (Eq, Data, Typeable)

instance Show Ty where
  show TyBool = "Bool"
  show (TyArr d c) = show d ++ " -> " ++ show c

data Binding
  = NameBind
  | VarBind Ty
  deriving (Eq, Show, Data, Typeable)

type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

getBinding :: Info -> Context -> Int -> Either String Binding
getBinding _ ctx i = maybe l (Right . snd) (lookup i (zip [0 ..] ctx))
  where
    l = Left ("Variable lookup failure: offset: " ++ show i ++ ", ctx size: " ++ show (length ctx))

getTypeFromContext :: Info -> Context -> Int -> Either String Ty
getTypeFromContext fi ctx i =
  case getBinding fi ctx i of
    Right (VarBind tyT) -> Right tyT
    Right _ -> Left "getTypeFromContext: Wrong kind of binding for variable"
    Left msg -> Left msg

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
printTmN (TmTrueN _) =
  "true"
printTmN (TmFalseN _) =
  "false"
printTmN (TmIfN _ p c a) =
  "if " ++ printTmN p ++ " then " ++ printTmN c ++ " else " ++ printTmN a
printTmN (TmVarN _ s) =
  s
printTmN (TmAbsN _ _ ty t) =
  "\\ " ++ show ty ++ ". " ++ printTmN t
printTmN (TmAppN _ a@(TmVarN {}) b@(TmAbsN {})) =
  printTmN a ++ " (" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmVarN {}) b@(TmAppN {})) =
  printTmN a ++ " (" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAppN {}) b@(TmAbsN {})) =
  printTmN a ++ " (" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAppN {}) b@(TmAppN {})) =
  printTmN a ++ " (" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAbsN {}) b@(TmAbsN {})) =
  "(" ++ printTmN a ++ ") " ++ "(" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAbsN {}) b@(TmAppN {})) =
  "(" ++ printTmN a ++ ") " ++ "(" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAbsN {}) b) =
  "(" ++ printTmN a ++ ") " ++ printTmN b
printTmN (TmAppN _ a b) =
  printTmN a ++ " " ++ printTmN b
printTmN (TmMetaVarN {}) =
  error "Attempting to print a metavariable"

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
printTmB (TmTrueB _) =
  "true"
printTmB (TmFalseB _) =
  "false"
printTmB (TmIfB _ p c a) =
  "if " ++ printTmB p ++ " then " ++ printTmB c ++ " else " ++ printTmB a
printTmB (TmVarB _ i _) =
  show i
printTmB (TmAbsB _ _ ty t) =
  "\\ " ++ show ty ++ ". " ++ printTmB t
printTmB (TmAppB _ a@(TmVarB {}) b@(TmAbsB {})) =
  printTmB a ++ " (" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmVarB {}) b@(TmAppB {})) =
  printTmB a ++ " (" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAppB {}) b@(TmAbsB {})) =
  printTmB a ++ " (" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAppB {}) b@(TmAppB {})) =
  printTmB a ++ " (" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAbsB {}) b@(TmAbsB {})) =
  "(" ++ printTmB a ++ ") " ++ "(" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAbsB {}) b@(TmAppB {})) =
  "(" ++ printTmB a ++ ") " ++ "(" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAbsB {}) b) =
  "(" ++ printTmB a ++ ") " ++ printTmB b
printTmB (TmAppB _ a b) =
  printTmB a ++ " " ++ printTmB b

instance Show TermB where
  show = printTmB

instance Eq TermB where
  TmTrueB _ == TmTrueB _ =
    True
  TmFalseB _ == TmFalseB _ =
    True
  TmIfB _ a b c == TmIfB _ n o p =
    a == n && b == o && c == p
  TmVarB _ a b == TmVarB _ n o =
    a == n && b == o
  TmAbsB _ a b c == TmAbsB _ n o p =
    a == n && b == o && c == p
  TmAppB _ a b == TmAppB _ n o =
    a == n && b == o
  _ == _ =
    False

termNtoB :: TermN -> TermB
termNtoB = to []
  where
    to :: [String] -> TermN -> TermB
    to _ (TmTrueN i) = TmTrueB i
    to _ (TmFalseN i) = TmFalseB i
    to l (TmIfN i p c a) = TmIfB i (to l p) (to l c) (to l a)
    to l (TmVarN i v) = maybe (TmVarB i 0 (length l)) (\x -> TmVarB i x (length l)) (elemIndex v l)
    to l (TmAbsN i x t b) = TmAbsB i x t (to (x : l) b)
    to l (TmAppN i f a) = TmAppB i (to l f) (to l a)
    to _ (TmMetaVarN {}) = error "Attempting to convert a metavariable to its de-Bruijn-indexed version"
