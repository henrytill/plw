module Language.Untyped.Lexer where

import           Data.Functor.Identity (Identity)
import           Text.Parsec           hiding (spaces)
import qualified Text.Parsec.Language  as Language
import           Text.Parsec.String    (Parser)
import qualified Text.Parsec.Token     as Token


symbol :: Parser Char
symbol = oneOf "!%&|*+-/:<=>?@^_~"

untypedDef :: Token.GenLanguageDef String () Identity
untypedDef = Language.emptyDef
  { Token.commentStart    = ""
  , Token.commentEnd      = ""
  , Token.commentLine     = ";"
  , Token.opStart         = Token.opLetter untypedDef
  , Token.opLetter        = symbol
  , Token.identStart      = letter <|> symbol
  , Token.identLetter     = letter <|> symbol <|> digit
  , Token.reservedOpNames = []
  }

lexer :: Token.GenTokenParser String () Identity
lexer = Token.makeTokenParser untypedDef

reservedOp :: String -> Parser ()
reservedOp op = Token.reservedOp lexer op

identifier :: Parser String
identifier = Token.identifier lexer

spaces :: Parser ()
spaces = Token.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer
