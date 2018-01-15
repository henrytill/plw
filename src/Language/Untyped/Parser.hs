module Language.Untyped.Parser (termP) where

import           Data.List               (elemIndex)
import           Language.Untyped.Base
import           Language.Untyped.Lambda
import           Text.Parsec
import           Text.Parsec.String      (Parser)


infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

lexeme :: Parser String -> Parser String
lexeme p = do
  x <- p
  _ <- spaces
  return x

lexemeString :: String -> Parser String
lexemeString = lexeme . string

identifier :: Parser String
identifier = lexeme $ do
  a <- letter
  b <- many (letter <|> digit)
  return (a : b)

parens :: Parser Term -> Parser Term
parens p = between (lexemeString "(") (lexemeString ")") p

absP :: Parser Term -> Parser Term
absP bodyP = do
  _   <- char '\\'
  v   <- identifier
  _   <- char '.'
  _   <- spaces
  b   <- bodyP
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v b

metaVarP :: Parser Term
metaVarP = do
  _   <- char '$'
  v   <- identifier
  pos <- getPosition
  return $ TmMetaVar (infoFrom pos) v

varP :: Parser Term
varP = do
  v   <- identifier
  pos <- getPosition
  return $ TmVar (infoFrom pos) v

nonAppP :: Parser Term
nonAppP = parens termP <|> absP termP <|> metaVarP <|> varP

appP :: Parsec String () (Term -> Term -> Term)
appP = do
  _   <- spaces
  pos <- getPosition
  return $ TmApp (infoFrom pos)

termP :: Parser Term
termP = nonAppP `chainl1` appP
