{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.Untyped.QuoteTests (quoteTests) where

import Language.Untyped.Core (eval)
import Language.Untyped.Quote (untyped)
import Language.Untyped.Syntax (TermN, termNtoB)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (and, fst, not, or, snd, succ)

-- * Examples

-- ** Church Booleans

tru, fls :: TermN
tru = [untyped| \t. \f. t |]
fls = [untyped| \t. \f. f |]

test, and, or, not :: TermN
test = [untyped| \l. \m. \n. l m n |]
and = [untyped| \b. \c. b c $fls |]
or = [untyped| \b. \c. b $tru c |]
not = [untyped| \b. b $fls $tru |]

-- ** Pairs

pair, fst, snd :: TermN
pair = [untyped| \f. \s. \b. b f s |]
fst = [untyped| \p. p $tru |]
snd = [untyped| \p. p $fls |]

-- ** Church Numerals

c0, c1, c2, c3 :: TermN
c0 = [untyped| \s. \z. z |]
c1 = [untyped| \s. \z. s z |]
c2 = [untyped| \s. \z. s s z |]
c3 = [untyped| \s. \z. s s s z |]

plus, times :: TermN
plus = [untyped| \m. \n. \s. \z. m s (n s z) |]
times = [untyped| \m. \n. m ($plus n) $c0 |]

isZro :: TermN
isZro = [untyped| \m. m (\x. $fls) $tru |]

zz, ss, prd :: TermN
zz = [untyped| $pair $c0 $c1 |]
ss = [untyped| \p. $pair ($snd p) ($plus $c1 ($snd p)) |]
prd = [untyped| \m. $fst (m $ss $zz) |]

equal :: TermN
equal = [untyped| \m. \n. $and ($isZro (m $prd n)) ($isZro (n $prd m)) |]

-- ** Recursion

omega :: TermN
omega = [untyped| (\x. x x) (\x. x x) |]

fix :: TermN
fix = [untyped| \f. (\x. f (\y. x x y)) (\x. f (\y. x x y)) |]

-- * Test Cases

testOnePlusOne :: TestTree
testOnePlusOne =
  testCase "plus c1 c1" $
    assertEqual "For the result of eval," expected actual
  where
    expected = eval [] $ termNtoB [untyped| \s. \z. (\s. \z. s z) s ((\s. \z. s z) s z) |]
    actual = eval [] $ termNtoB [untyped| $plus $c1 $c1 |]

testIdentityOfSelfApplication :: TestTree
testIdentityOfSelfApplication =
  testCase "(\\.x x) (\\x. x x)" $
    assertEqual "For the result of eval," expected actual
  where
    expected = eval [] $ termNtoB [untyped| \x. x x |]
    actual = eval [] $ termNtoB [untyped| (\x. x) (\x. x x) |]

quoteTests :: TestTree
quoteTests =
  testGroup
    "Untyped Lambda Calculus"
    [ testOnePlusOne
    , testIdentityOfSelfApplication
    ]
