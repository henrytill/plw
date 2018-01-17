module Language.Untyped.Parser (termP) where

import           Language.Base.Parser
import           Language.Untyped.Syntax
import           Text.Parsec
import           Text.Parsec.String      (Parser)


absP :: Parser TermN -> Parser TermN
absP bodyP = do
  _   <- char '\\'
  v   <- identifier
  _   <- char '.'
  _   <- spaces
  b   <- bodyP
  pos <- getPosition
  return $ TmAbsN (infoFrom pos) v b

metaVarP :: Parser TermN
metaVarP = do
  _   <- char '$'
  v   <- identifier
  pos <- getPosition
  return $ TmMetaVarN (infoFrom pos) v

varP :: Parser TermN
varP = do
  v   <- identifier
  pos <- getPosition
  return $ TmVarN (infoFrom pos) v

nonAppP :: Parser TermN
nonAppP = parens termP <|> absP termP <|> metaVarP <|> varP

appP :: Parsec String () (TermN -> TermN -> TermN)
appP = do
  _   <- spaces
  pos <- getPosition
  return $ TmAppN (infoFrom pos)

termP :: Parser TermN
termP = nonAppP `chainl1` appP
