module Language.SimpleBool.Core where

import Language.Base (Info)
import Language.SimpleBool.Syntax

typeOf :: Context -> TermB -> Either String Ty
typeOf _ (TmTrueB _) = Right TyBool
typeOf _ (TmFalseB _) = Right TyBool
typeOf ctx (TmIfB _ t1 t2 t3) = do
  tyT1 <- typeOf ctx t1
  if tyT1 /= TyBool
    then Left "guard of conditional not a boolean"
    else do
      tyT2 <- typeOf ctx t2
      tyT3 <- typeOf ctx t3
      if tyT2 /= tyT3
        then Left "arms of conditional have different types"
        else return tyT2
typeOf ctx (TmVarB fi i _) = getTypeFromContext fi ctx i
typeOf ctx (TmAbsB _ x tyT1 t2) = TyArr tyT1 <$> typeOf (addBinding ctx x (VarBind tyT1)) t2
typeOf ctx (TmAppB _ t1 t2) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  case tyT1 of
    TyArr tyT11 tyT12
      | tyT2 == tyT11 -> Right tyT12
      | otherwise -> Left "parameter type mismatch"
    _ -> Left "arrow type expected"

isVal :: t -> TermB -> Bool
isVal _ (TmFalseB _) = True
isVal _ (TmTrueB _) = True
isVal _ (TmAbsB {}) = True
isVal _ _ = False

tmMap :: (Num t) => (Info -> t -> Int -> Int -> TermB) -> t -> TermB -> TermB
tmMap onVar = walk
  where
    walk f (TmVarB fi x n) = onVar fi f x n
    walk f (TmAbsB fi x tyT1 t2) = TmAbsB fi x tyT1 (walk (f + 1) t2)
    walk f (TmAppB fi t1 t2) = TmAppB fi (walk f t1) (walk f t2)
    walk _ x@(TmTrueB _) = x
    walk _ x@(TmFalseB _) = x
    walk f (TmIfB fi t1 t2 t3) = TmIfB fi (walk f t1) (walk f t2) (walk f t3)

termShiftAbove :: Int -> Int -> TermB -> TermB
termShiftAbove d = tmMap f
  where
    f fi cc x n
      | x >= cc = TmVarB fi (x + d) (n + d)
      | otherwise = TmVarB fi x (n + d)

termShift :: Int -> TermB -> TermB
termShift d = termShiftAbove d 0

termSubst :: Int -> TermB -> TermB -> TermB
termSubst j s = tmMap f j
  where
    f fi jj x n
      | x == jj = termShift jj s
      | otherwise = TmVarB fi x n

termSubstTop :: TermB -> TermB -> TermB
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
