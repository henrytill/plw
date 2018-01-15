{-# LANGUAGE QuasiQuotes #-}

module Language.Untyped.QuoteExamples where

import           Language.Untyped.Lambda (Term)
import           Language.Untyped.Quote  (untyped)
import           Prelude                 hiding (and, fst, not, or, snd, succ)

-- * Church Booleans

tru, fls :: Term
tru = [untyped| \t. \f. t |]
fls = [untyped| \t. \f. f |]

test, and, or, not :: Term
test = [untyped| \l. \m. \n. l m n |]
and  = [untyped| \b. \c. b c $fls |]
or   = [untyped| \b. \c. b $tru c |]
not  = [untyped| \b. b $fls $tru |]

-- * Pairs

pair, fst, snd :: Term
pair = [untyped| \f. \s. \b. b f s |]
fst  = [untyped| \p. p $tru |]
snd  = [untyped| \p. p $fls |]

-- * Church Numerals

c0, c1, c2, c3 :: Term
c0 = [untyped| \s. \z. z |]
c1 = [untyped| \s. \z. s z |]
c2 = [untyped| \s. \z. s s z |]
c3 = [untyped| \s. \z. s s s z |]

plus, times :: Term
plus  = [untyped| \m. \n. \s. \z. m s (n s z) |]
times = [untyped| \m. \n. m (plus n) $c0 |]

isZro :: Term
isZro = [untyped| \m. m (\x. $fls) $tru |]

zz, ss, prd :: Term
zz  = [untyped| $pair $c0 $c1 |]
ss  = [untyped| \p. $pair ($snd p) ($plus $c1 ($snd p)) |]
prd = [untyped| \m. $fst (m $ss $zz) |]

equal :: Term
equal = [untyped| \m. \n. $and ($isZro (m $prd n)) ($isZro (n $prd m)) |]

-- * Recursion

omega :: Term
omega = [untyped| (\x. x x) (\x. x x) |]

fix :: Term
fix = [untyped| \f. (\x. f (\y. x x y)) (\x. f (\y. x x y)) |]
