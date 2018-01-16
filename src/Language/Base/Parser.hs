module Language.Base.Parser where

import           Language.Base
import           Text.Parsec
import           Text.Parsec.String (Parser)


infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

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

typeIdentifer :: Parser String
typeIdentifer = lexeme $ do
  a <- upper
  b <- many alphaNum
  return (a : b)

identifier :: Parser String
identifier = lexeme $ do
  a <- lower
  b <- many alphaNum
  return (a : b)

parens :: Parser t -> Parser t
parens p = between (lexemeString "(") (lexemeString ")") p
