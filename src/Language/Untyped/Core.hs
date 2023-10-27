module Language.Untyped.Core (eval) where

import Language.Untyped.Syntax (TermB (..))


termShift :: Int -> TermB -> TermB
termShift d t = walk 0 t
  where
    walk c (TmVarB fi x n)
      | x >= c = TmVarB fi (x + d) (n + d)
      | otherwise = TmVarB fi x (n + d)
    walk c (TmAbsB fi x t1) = TmAbsB fi x (walk (c + 1) t1)
    walk c (TmAppB fi t1 t2) = TmAppB fi (walk c t1) (walk c t2)

termSubst :: Int -> TermB -> TermB -> TermB
termSubst j s t = walk 0 t
  where
    walk c (TmVarB fi x n)
      | x == j + c = termShift c s
      | otherwise = TmVarB fi x n
    walk c (TmAbsB fi x t1) = TmAbsB fi x (walk (c + 1) t1)
    walk c (TmAppB fi t1 t2) = TmAppB fi (walk c t1) (walk c t2)

termSubstTop :: TermB -> TermB -> TermB
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: t -> TermB -> Bool
isVal _ (TmAbsB _ _ _) = True
isVal _ _ = False

eval1 :: t -> TermB -> Maybe TermB
eval1 ctx (TmAppB _  (TmAbsB _ _ t12) v2) | isVal ctx v2 = pure (termSubstTop v2 t12)
eval1 ctx (TmAppB fi v1 t2) | isVal ctx v1 = TmAppB <$> pure fi <*> pure v1 <*> eval1 ctx t2
eval1 ctx (TmAppB fi t1 t2) = TmAppB <$> pure fi <*> eval1 ctx t1 <*> pure t2
eval1 _ _ = Nothing

eval :: t -> TermB -> Maybe TermB
eval ctx t =
  case eval1 ctx t of
    Nothing -> return t
    Just t' -> eval ctx t'
