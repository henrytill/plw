module Language.SimpleBool.Core where

import           Language.SimpleBool.Syntax


typeOf :: Context -> TermB -> Either String Ty
typeOf _   (TmTrueB _)        = Right TyBool
typeOf _   (TmFalseB _)       = Right TyBool
typeOf ctx (TmIfB _ t1 t2 t3) = do
  tyT1 <- typeOf ctx t1
  if tyT1 /= TyBool
    then (Left "guard of conditional not a boolean")
    else do
    tyT2 <- typeOf ctx t2
    tyT3 <- typeOf ctx t3
    if tyT2 /= tyT3
      then (Left "arms of conditional have different types")
      else return tyT2
typeOf ctx (TmVarB fi i _)      = getTypeFromContext fi ctx i
typeOf ctx (TmAbsB _ x tyT1 t2) = TyArr <$> pure tyT1 <*> typeOf (addBinding ctx x (VarBind tyT1)) t2
typeOf ctx (TmAppB _ t1 t2)     = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  case tyT1 of
    TyArr tyT11 tyT12 | tyT2 == tyT11 -> Right tyT12
                      | otherwise     -> Left "parameter type mismatch"
    _                                 -> Left "arrow type expected"
