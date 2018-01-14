{-# LANGUAGE QuasiQuotes #-}

module Language.Untyped.QuoteExamples where

import           Language.Untyped.Lambda (Term)
import           Language.Untyped.Quote  (untyped)

ex :: Term
ex = [untyped| \y.x y |]

metaVar :: Term -> Term
metaVar exp = [untyped| \x.$exp |]
