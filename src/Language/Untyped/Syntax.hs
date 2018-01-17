{-# LANGUAGE DeriveDataTypeable #-}

module Language.Untyped.Syntax
  ( TermN (..)
  , TermB (..)
  , termNtoB
  ) where

import           Data.Data
import           Data.List     (elemIndex)
import           Language.Base (Info)


-- * String-based names

data TermN
  = TmVarN Info String
  | TmAbsN Info String TermN
  | TmAppN Info TermN TermN
  | TmMetaVarN Info String
  deriving (Eq, Data, Typeable)

printTmN :: TermN -> String
printTmN (TmVarN _ v)                                   = v
printTmN (TmAbsN _ v t)                                 = "\\" ++ v ++ ". " ++ printTmN t
printTmN (TmAppN _ t1@(TmVarN _ _)   t2@(TmAbsN _ _ _)) =        printTmN t1 ++        " (" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmVarN _ _)   t2@(TmAppN _ _ _)) =        printTmN t1 ++        " (" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAppN _ _ _) t2@(TmAbsN _ _ _)) =        printTmN t1 ++        " (" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAppN _ _ _) t2@(TmAppN _ _ _)) =        printTmN t1 ++        " (" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAbsN _ _ _) t2@(TmAbsN _ _ _)) = "(" ++ printTmN t1 ++ ") " ++ "(" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAbsN _ _ _) t2@(TmAppN _ _ _)) = "(" ++ printTmN t1 ++ ") " ++ "(" ++ printTmN t2 ++ ")"
printTmN (TmAppN _ t1@(TmAbsN _ _ _) t2)                = "(" ++ printTmN t1 ++ ") "        ++ printTmN t2
printTmN (TmAppN _ t1                t2)                =        printTmN t1 ++ " "         ++ printTmN t2
printTmN (TmMetaVarN _ _)                               = error "Attempting to print a metavariable"

instance Show TermN where
  show = printTmN

-- * de-Bruijn indices

data TermB
  = TmVarB Info Int Int
  | TmAbsB Info String TermB
  | TmAppB Info TermB TermB
  deriving (Data, Typeable)

printTmB :: TermB -> String
printTmB (TmVarB _ i _)                                 = show i
printTmB (TmAbsB _ _ t)                                 = "\\. " ++ printTmB t
printTmB (TmAppB _ t1@(TmVarB _ _ _) t2@(TmAbsB _ _ _)) =        printTmB t1 ++        " (" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmVarB _ _ _) t2@(TmAppB _ _ _)) =        printTmB t1 ++        " (" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAppB _ _ _) t2@(TmAbsB _ _ _)) =        printTmB t1 ++        " (" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAppB _ _ _) t2@(TmAppB _ _ _)) =        printTmB t1 ++        " (" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAbsB _ _ _) t2@(TmAbsB _ _ _)) = "(" ++ printTmB t1 ++ ") " ++ "(" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAbsB _ _ _) t2@(TmAppB _ _ _)) = "(" ++ printTmB t1 ++ ") " ++ "(" ++ printTmB t2 ++ ")"
printTmB (TmAppB _ t1@(TmAbsB _ _ _) t2)                = "(" ++ printTmB t1 ++ ") "        ++ printTmB t2
printTmB (TmAppB _ t1               t2)                 =        printTmB t1 ++ " "         ++ printTmB t2

instance Show TermB where
  show = printTmB

instance Eq TermB where
  TmVarB _ i1 t1 == TmVarB _ i2 t2 = i1 == i2 && t1 == t2
  TmAbsB _ s1 t1 == TmAbsB _ s2 t2 = s1 == s2 && t1 == t2
  TmAppB _ l1 l2 == TmAppB _ r1 r2 = l1 == r1 && l2 == r2
  _              == _              = False

termNtoB :: TermN -> TermB
termNtoB = to []
  where
    to :: [String] -> TermN -> TermB
    to vs (TmVarN info v)   = maybe (TmVarB info 0 (length vs))
                                    (\x -> TmVarB info x (length vs))
                                    (elemIndex v vs)
    to vs (TmAbsN info x b) = TmAbsB info x (to (x : vs) b)
    to vs (TmAppN info f a) = TmAppB info (to vs f) (to vs a)
    to _  (TmMetaVarN _ _)  = error "Attempting to convert a metavariable to its de-Bruijn-indexed version"
