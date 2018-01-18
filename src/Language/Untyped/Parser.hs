module Language.Untyped.Parser (termP) where

import           Language.Base
import           Language.Untyped.Syntax
import           Text.Parsec
import           Text.Parsec.String      (Parser)


-- |
-- <https://www.reddit.com/r/haskell/comments/2ia5u2/after_some_failed_attempts_to_learn_parsec_i_came/cl0c3b7/>
--
lexeme :: Parser String -> Parser String
lexeme p = do
  x <- p
  _ <- spaces
  return x

lexemeString :: String -> Parser String
lexemeString = lexeme . string

identifier :: Parser String
identifier = lexeme $ do
  a <- lower
  b <- many alphaNum
  return (a : b)

parens :: Parser t -> Parser t
parens p = between (lexemeString "(") (lexemeString ")") p

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
