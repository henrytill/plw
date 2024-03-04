module Language.Untyped.Quote (untyped) where

import Data.Generics.Aliases (extQ)
import Language.Base.Quote
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Language.Untyped.Parser (termP)
import Language.Untyped.Syntax (TermN (TmMetaVarN))
import Text.Parsec (SourcePos, setPosition)

parseTerm :: SourcePos -> String -> TH.Q TermN
parseTerm pos str = parseOrError (setPosition pos *> topLevel termP) "untyped lambda calculus" str

antiExpLambda :: TermN -> Maybe (TH.Q TH.Exp)
antiExpLambda (TmMetaVarN _ v) = Just (TH.varE (TH.mkName v))
antiExpLambda _ = Nothing

antiPatLambda :: TermN -> Maybe (TH.Q TH.Pat)
antiPatLambda (TmMetaVarN _ v) = Just (TH.varP (TH.mkName v))
antiPatLambda _ = Nothing

quoteExpLambda :: String -> TH.Q TH.Exp
quoteExpLambda str = do
  pos <- getSourcePos
  term <- parseTerm pos str
  dataToExpQ (const Nothing `extQ` antiExpLambda) term

quotePatLambda :: String -> TH.Q TH.Pat
quotePatLambda str = do
  pos <- getSourcePos
  term <- parseTerm pos str
  dataToPatQ (const Nothing `extQ` antiPatLambda) term

untyped :: QuasiQuoter
untyped =
  QuasiQuoter
    { quoteExp = quoteExpLambda,
      quotePat = quotePatLambda,
      quoteType = undefined,
      quoteDec = undefined
    }
