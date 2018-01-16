module Language.SimpleBool.Quote (simpleBool) where

import           Data.Generics.Aliases      (extQ)
import           Language.Base.Quote
import qualified Language.Haskell.TH        as TH
import           Language.Haskell.TH.Quote
import           Language.SimpleBool.Parser (termP)
import           Language.SimpleBool.Syntax (Term (TmMetaVar))
import           Text.Parsec                (SourcePos, setPosition)


parseTerm :: Monad m => SourcePos -> String -> m Term
parseTerm pos str = parseOrError (setPosition pos *> topLevel termP) "simply-typed lambda calculus" str

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

simpleBool :: QuasiQuoter
simpleBool = QuasiQuoter
  { quoteExp  = quoteExpLambda
  , quotePat  = quotePatLambda
  , quoteType = undefined
  , quoteDec  = undefined
  }
