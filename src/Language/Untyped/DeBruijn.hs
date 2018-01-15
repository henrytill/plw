{-# LANGUAGE DeriveDataTypeable #-}

module Language.Untyped.DeBruijn where

import           Data.Data
import           Data.List               (elemIndex)
import           Language.Base           (Info)
import qualified Language.Untyped.Lambda as Lambda


data Term
  = TmVar Info !Int !Int
  | TmAbs Info String Term
  | TmApp Info Term Term
  deriving (Data, Typeable)

printTm :: Term -> String
printTm (TmVar _ i _)                               = show i
printTm (TmAbs _ _ t)                               = "\\. " ++ printTm t
printTm (TmApp _ t1@(TmVar _ _ _) t2@(TmAbs _ _ _)) =        printTm t1 ++        " (" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmVar _ _ _) t2@(TmApp _ _ _)) =        printTm t1 ++        " (" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmApp _ _ _) t2@(TmAbs _ _ _)) =        printTm t1 ++        " (" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmApp _ _ _) t2@(TmApp _ _ _)) =        printTm t1 ++        " (" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmAbs _ _ _) t2@(TmAbs _ _ _)) = "(" ++ printTm t1 ++ ") " ++ "(" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmAbs _ _ _) t2@(TmApp _ _ _)) = "(" ++ printTm t1 ++ ") " ++ "(" ++ printTm t2 ++ ")"
printTm (TmApp _ t1@(TmAbs _ _ _) t2)               = "(" ++ printTm t1 ++ ") "        ++ printTm t2
printTm (TmApp _ t1               t2)               =        printTm t1 ++ " "         ++ printTm t2

instance Show Term where
  show = printTm

instance Eq Term where
  TmVar _ i1 t1 == TmVar _ i2 t2 = i1 == i2 && t1 == t2
  TmAbs _ s1 t1 == TmAbs _ s2 t2 = s1 == s2 && t1 == t2
  TmApp _ l1 l2 == TmApp _ r1 r2 = l1 == r1 && l2 == r2
  _             == _             = False

toDeBruijn :: Lambda.Term -> Term
toDeBruijn = to []
  where
    to :: [String] -> Lambda.Term -> Term
    to vs (Lambda.TmVar info v)   = maybe (TmVar info 0 (length vs))
                                          (\x -> TmVar info x (length vs))
                                          (elemIndex v vs)
    to vs (Lambda.TmAbs info x b) = TmAbs info x (to (x : vs) b)
    to vs (Lambda.TmApp info f a) = TmApp info (to vs f) (to vs a)
    to _  (Lambda.TmMetaVar _ _)  = error "Attemtping to convert a metavariable to its DeBruijn-indexed version"

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
  where
    walk c (TmVar fi x  n)
      | x >= c              = TmVar fi (x + d)     (n + d)
      | otherwise           = TmVar fi x           (n + d)
    walk c (TmAbs fi x  t1) = TmAbs fi x           (walk (c + 1) t1)
    walk c (TmApp fi t1 t2) = TmApp fi (walk c t1) (walk c       t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
  where
    walk c (TmVar fi x  n)
      | x == j + c          = termShift c s
      | otherwise           = TmVar fi x           n
    walk c (TmAbs fi x  t1) = TmAbs fi x           (walk (c + 1) t1)
    walk c (TmApp fi t1 t2) = TmApp fi (walk c t1) (walk c       t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: t -> Term -> Bool
isVal _ (TmAbs _ _ _) = True
isVal _ _             = False

eval1 :: t -> Term -> Maybe Term
eval1 ctx (TmApp _  (TmAbs _ _ t12) v2) | isVal ctx v2 = pure (termSubstTop v2 t12)
eval1 ctx (TmApp fi v1              t2) | isVal ctx v1 = TmApp <$> pure fi <*> pure v1      <*> eval1 ctx t2
eval1 ctx (TmApp fi t1              t2)                = TmApp <$> pure fi <*> eval1 ctx t1 <*> pure t2
eval1 _   _                                            = Nothing

eval :: t -> Term -> Maybe Term
eval ctx t =
  case eval1 ctx t of
    Nothing -> return t
    Just t' -> eval ctx t'
