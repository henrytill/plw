module Language.Untyped.HOAS where


data TmExp
  = TmApp TmExp TmExp
  | TmAbs (TmExp -> TmExp)

eval :: TmExp -> TmExp
eval = nfh

nfh :: TmExp -> TmExp
nfh (TmAbs b)   = TmAbs (nfh . b)
nfh (TmApp f a) =
  case whnf f of
    TmAbs b -> nfh (b a)
    f'     -> TmApp (nfh f') (nfh a)

whnf :: TmExp -> TmExp
whnf e@(TmAbs  _) = e
whnf (TmApp f a)  =
  case whnf f of
    TmAbs b -> whnf (b a)
    f'     -> TmApp f' a
