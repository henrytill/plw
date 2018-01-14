{-# LANGUAGE DeriveDataTypeable #-}

module Language.Untyped.DeBruijn where

import           Data.Data
import           Data.Typeable
import           Language.Untyped.Base (Info)


data Term
  = TmVar Info !Int !Int
  | TmAbs Info String Term
  | TmApp Info Term Term
  deriving (Eq, Show, Data, Typeable)

data NoRuleApplies = MkNoRuleApplies

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

eval1 :: t -> Term -> Either NoRuleApplies Term
eval1 ctx (TmApp _  (TmAbs _ _ t12) v2) | isVal ctx v2 = pure (termSubstTop v2 t12)
eval1 ctx (TmApp fi v1              t2) | isVal ctx v1 = TmApp <$> pure fi <*> pure v1      <*> eval1 ctx t2
eval1 ctx (TmApp fi t1              t2)                = TmApp <$> pure fi <*> eval1 ctx t1 <*> pure t2
eval1 _   _                                            = Left MkNoRuleApplies

eval :: t -> Term -> Either NoRuleApplies Term
eval ctx t = do
  t' <- eval1 ctx t
  case eval ctx t' of
    Right r -> return r
    Left  _ -> return t
