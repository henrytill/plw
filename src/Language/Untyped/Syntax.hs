{-# LANGUAGE DeriveDataTypeable #-}

module Language.Untyped.Syntax
  ( TermN (..)
  , TermB (..)
  , termNtoB
  ) where

import Data.Data
import Data.List (elemIndex)
import Language.Base (Info)


-- * String-based names

data TermN
  = TmVarN Info String
  | TmAbsN Info String TermN
  | TmAppN Info TermN TermN
  | TmMetaVarN Info String
  deriving (Data, Typeable)

printTmN :: TermN -> String
printTmN (TmVarN _ v) =
  v
printTmN (TmAbsN _ v t) =
  "\\" ++ v ++ ". " ++ printTmN t
printTmN (TmAppN _ a@(TmVarN _ _) b@(TmAbsN _ _ _)) =
  printTmN a ++ " (" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmVarN _ _) b@(TmAppN _ _ _)) =
  printTmN a ++ " (" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAppN _ _ _) b@(TmAbsN _ _ _)) =
  printTmN a ++ " (" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAppN _ _ _) b@(TmAppN _ _ _)) =
  printTmN a ++ " (" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAbsN _ _ _) b@(TmAbsN _ _ _)) =
  "(" ++ printTmN a ++ ") " ++ "(" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAbsN _ _ _) b@(TmAppN _ _ _)) =
  "(" ++ printTmN a ++ ") " ++ "(" ++ printTmN b ++ ")"
printTmN (TmAppN _ a@(TmAbsN _ _ _) b) =
  "(" ++ printTmN a ++ ") " ++ printTmN b
printTmN (TmAppN _ a b) =
  printTmN a ++ " " ++ printTmN b
printTmN (TmMetaVarN _ _) =
  error "Attempting to print a metavariable"

instance Show TermN where
  show = printTmN

-- * de-Bruijn indices

data TermB
  = TmVarB Info Int Int
  | TmAbsB Info String TermB
  | TmAppB Info TermB TermB
  deriving (Data, Typeable)

printTmB :: TermB -> String
printTmB (TmVarB _ i _) =
  show i
printTmB (TmAbsB _ _ t) =
  "\\. " ++ printTmB t
printTmB (TmAppB _ a@(TmVarB _ _ _) b@(TmAbsB _ _ _)) =
  printTmB a ++ " (" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmVarB _ _ _) b@(TmAppB _ _ _)) =
  printTmB a ++ " (" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAppB _ _ _) b@(TmAbsB _ _ _)) =
  printTmB a ++ " (" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAppB _ _ _) b@(TmAppB _ _ _)) =
  printTmB a ++ " (" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAbsB _ _ _) b@(TmAbsB _ _ _)) =
  "(" ++ printTmB a ++ ") " ++ "(" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAbsB _ _ _) b@(TmAppB _ _ _)) =
  "(" ++ printTmB a ++ ") " ++ "(" ++ printTmB b ++ ")"
printTmB (TmAppB _ a@(TmAbsB _ _ _) b) =
  "(" ++ printTmB a ++ ") " ++ printTmB b
printTmB (TmAppB _ a b) =
  printTmB a ++ " " ++ printTmB b

instance Show TermB where
  show = printTmB

instance Eq TermB where
  TmVarB _ a b == TmVarB _ n o =
    a == n && b == o

  TmAbsB _ a b == TmAbsB _ n o =
    a == n && b == o

  TmAppB _ a b == TmAppB _ n o =
    a == n && b == o

  _ == _ =
    False

termNtoB :: TermN -> TermB
termNtoB = to []
  where
    to :: [String] -> TermN -> TermB
    to l (TmVarN i v) = maybe (TmVarB i 0 (length l)) (\ x -> TmVarB i x (length l)) (elemIndex v l)
    to l (TmAbsN i x b) = TmAbsB i x (to (x : l) b)
    to l (TmAppN i f a) = TmAppB i (to l f) (to l a)
    to _ (TmMetaVarN _ _) = error "Attempting to convert a metavariable to its de-Bruijn-indexed version"
