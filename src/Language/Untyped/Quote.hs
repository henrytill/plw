module Language.Untyped.Quote (untyped) where

import           Data.Generics.Aliases     (extQ)
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote
import           Language.Base.Quote
import           Language.Untyped.Lambda   (Term (TmMetaVar))
import           Language.Untyped.Parser   (termP)
import           Text.Parsec               (SourcePos, setPosition)


parseTerm :: Monad m => SourcePos -> String -> m Term
parseTerm pos str = parseOrError (setPosition pos *> topLevel termP) "untyped lambda calculus" str

antiExpLambda :: Term -> Maybe (TH.Q TH.Exp)
antiExpLambda (TmMetaVar _ v) = Just (TH.varE (TH.mkName v))
antiExpLambda _               = Nothing

antiPatLambda :: Term -> Maybe (TH.Q TH.Pat)
antiPatLambda (TmMetaVar _ v) = Just (TH.varP (TH.mkName v))
antiPatLambda _               = Nothing

quoteExpLambda :: String -> TH.Q TH.Exp
quoteExpLambda str = do
  pos  <- getSourcePos
  term <- parseTerm pos str
  dataToExpQ (const Nothing `extQ` antiExpLambda) term

quotePatLambda :: String -> TH.Q TH.Pat
quotePatLambda str = do
  pos  <- getSourcePos
  term <- parseTerm pos str
  dataToPatQ (const Nothing `extQ` antiPatLambda) term

untyped :: QuasiQuoter
untyped = QuasiQuoter
  { quoteExp  = quoteExpLambda
  , quotePat  = quotePatLambda
  , quoteType = undefined
  , quoteDec  = undefined
  }
