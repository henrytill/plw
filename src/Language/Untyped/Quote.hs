module Language.Untyped.Quote where

import           Data.Generics.Aliases     (extQ)
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote
import           Language.Untyped.Lambda   (Term (TmMetaVar))
import qualified Language.Untyped.Lexer    as Lexer
import           Language.Untyped.Parser   (termP)
import           Text.Parsec
import           Text.Parsec.Pos           (newPos)
import           Text.Parsec.String        (Parser)


getSourcePos :: TH.Q SourcePos
getSourcePos = f <$> TH.location
  where
    f :: TH.Loc -> SourcePos
    f loc = uncurry (newPos (TH.loc_filename loc)) (TH.loc_start loc)

parseExp :: Monad m => SourcePos -> String -> m Term
parseExp pos str = parseOrError (setPosition pos *> topLevel termP) "untyped lambda calculus" str
  where
    parseOrError :: Monad m => Parser Term -> String -> String -> m Term
    parseOrError p name str = either (error . show) return (runParser p () name str)

    topLevel :: Parser Term -> Parser Term
    topLevel p = Lexer.spaces *> p <* eof

antiExpLambda :: Term -> Maybe (TH.Q TH.Exp)
antiExpLambda (TmMetaVar _ v) = Just (TH.varE (TH.mkName v))
antiExpLambda _               = Nothing

antiPatLambda :: Term -> Maybe (TH.Q TH.Pat)
antiPatLambda (TmMetaVar _ v) = Just (TH.varP (TH.mkName v))
antiPatLambda _               = Nothing

quoteExpLambda :: String -> TH.Q TH.Exp
quoteExpLambda str = do
  l <- getSourcePos
  c <- parseExp l str
  dataToExpQ (const Nothing `extQ` antiExpLambda) c

quotePatLambda :: String -> TH.Q TH.Pat
quotePatLambda str = do
  l <- getSourcePos
  c <- parseExp l str
  dataToPatQ (const Nothing `extQ` antiPatLambda) c

untyped :: QuasiQuoter
untyped = QuasiQuoter
  { quoteExp  = quoteExpLambda
  , quotePat  = quotePatLambda
  , quoteType = undefined
  , quoteDec  = undefined
  }
