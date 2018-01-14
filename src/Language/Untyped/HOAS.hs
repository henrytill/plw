{-# LANGUAGE StandaloneDeriving #-}

module Language.Untyped.HOAS where

import           Prelude hiding (and, fst, not, or, snd, succ)


data UExp
  = UApp UExp UExp
  | UAbs (UExp -> UExp)
  | UBool Bool
  | UInt Int

unwrapBool :: UExp -> Bool
unwrapBool (UBool b) = b
unwrapBool _         = error "can't unwrap"

unwrapInt :: UExp -> Int
unwrapInt (UInt i) = i
unwrapInt _        = error "can't unwrap"

succ :: UExp -> UExp
succ (UInt i) = UInt (i + 1)
succ _        = error "can't succ"

-- * Church Booleans

tru, fls :: UExp
tru = UAbs (\ t -> UAbs (\ _ -> t))
fls = UAbs (\ _ -> UAbs (\ f -> f))

test, and, or, not :: UExp
test = UAbs (\ l -> UAbs (\ m -> UAbs (\ n -> UApp (UApp l m) n)))
and  = UAbs (\ b -> UAbs (\ c -> UApp (UApp b c) fls))
or   = UAbs (\ b -> UAbs (\ c -> UApp (UApp b tru) c))
not  = UAbs (\ b -> UApp (UApp b fls) tru)

-- * Pairs

pair, fst, snd :: UExp
pair = UAbs (\ f -> UAbs (\ s -> UAbs (\ b -> UApp (UApp b f) s)))
fst  = UAbs (\ p -> UApp p tru)
snd  = UAbs (\ p -> UApp p fls)

-- * Church Numerals

c0, c1, c2, c3 :: UExp
c0 = UAbs (\ _ -> UAbs (\ z -> z))
c1 = UAbs (\ s -> UAbs (\ z -> UApp s z))
c2 = UAbs (\ s -> UAbs (\ z -> UApp s (UApp s z)))
c3 = UAbs (\ s -> UAbs (\ z -> UApp s (UApp s (UApp s z))))

plus, times :: UExp
plus  = UAbs (\ m -> UAbs (\ n -> UAbs (\ s -> UAbs (\ z -> UApp (UApp m s) (UApp (UApp n s) z)))))
times = UAbs (\ m -> UAbs (\ n -> UApp (UApp m (UApp plus n)) c0))

isZro :: UExp
isZro = UAbs (\ m -> UApp (UApp m (UAbs (\ _ -> fls))) tru)

zz, ss, prd :: UExp
zz  = UApp (UApp pair c0) c0
ss  = UAbs (\ p -> UApp (UApp pair (UApp snd p)) (UApp (UApp plus c1) (UApp snd p)))
prd = UAbs (\ m -> UApp fst (UApp (UApp m ss) zz))

equal :: UExp
equal = UAbs $ \ m ->
                 UAbs $ \ n ->
                          UApp
                          (UApp and (UApp isZro (UApp (UApp m prd) n)))
                          (UApp isZro (UApp (UApp n prd) m))

eval :: UExp -> UExp
eval = nfh

nfh :: UExp -> UExp
nfh e@(UBool _) = e
nfh e@(UInt  _) = e
nfh (UAbs b)    = UAbs (nfh . b)
nfh (UApp f a)  =
  case whnf f of
    UAbs b -> nfh (b a)
    f'     -> UApp (nfh f') (nfh a)

whnf :: UExp -> UExp
whnf e@(UBool _) = e
whnf e@(UInt  _) = e
whnf e@(UAbs  _) = e
whnf (UApp f a)  =
  case whnf f of
    UAbs b -> whnf (b a)
    f'     -> UApp f' a

realbool :: UExp
realbool = UAbs (\ b -> UApp (UApp b (UBool True)) (UBool False))

churchbool :: UExp
churchbool = UAbs (\ (UBool b) -> if b then tru else fls)

realeq :: UExp
realeq = UAbs (\ m -> UAbs (\ n -> (UApp (UApp (UApp (UApp equal m) n) (UBool True)) (UBool False))))

realnat :: UExp
realnat = UAbs (\ m -> UApp (UApp m (UAbs (\ x -> succ x))) (UInt 0))

fix :: UExp
fix = UAbs $ \ f ->
               UApp
               (UAbs (\ x -> UApp f (UAbs (\ y -> UApp (UApp x x) y))))
               (UAbs (\ x -> UApp f (UAbs (\ y -> UApp (UApp x x) y))))

g :: UExp
g = UAbs $ \ fct ->
             UAbs $ \n ->
                       UApp
                       (UApp (UApp test (UApp (UApp realeq n) c0)) c1)
                       (UApp (UApp times n) (UApp fct (UApp prd n)))
factorial :: UExp
factorial = UApp fix g
