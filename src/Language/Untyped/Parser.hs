module Language.Untyped.Parser (termP) where

import           Data.List               (elemIndex)
import           Language.Untyped.Base
import           Language.Untyped.Lambda
import qualified Language.Untyped.Lexer  as Lexer
import           Text.Parsec             hiding (spaces)
import           Text.Parsec.String      (Parser)


infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

absP :: Parser Term -> Parser Term
absP bodyP = do
  _   <- char '\\'
  v   <- Lexer.identifier
  _   <- char '.'
  _   <- Lexer.spaces
  b   <- bodyP
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v b

metaVarP :: Parser Term
metaVarP = do
  _   <- char '$'
  v   <- Lexer.identifier
  pos <- getPosition
  return $ TmMetaVar (infoFrom pos) v

varP :: Parser Term
varP = do
  v   <- Lexer.identifier
  pos <- getPosition
  return $ TmVar (infoFrom pos) v

nonAppP :: Parser Term
nonAppP = Lexer.parens termP <|> absP termP <|> metaVarP <|> varP

appP :: Parsec String () (Term -> Term -> Term)
appP = do
  Lexer.spaces
  pos <- getPosition
  return $ TmApp (infoFrom pos)

termP :: Parser Term
termP = nonAppP `chainl1` appP
