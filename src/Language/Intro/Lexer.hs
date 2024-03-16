module Language.Intro.Lexer where

import Text.Parsec.Language (LanguageDef)
import Text.Parsec.Language qualified as Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser)
import Text.Parsec.Token qualified as Token

langDef :: LanguageDef a
langDef =
  Language.emptyDef
    { Token.reservedOpNames = ["+", "*", "^", "-"]
    }

lexer :: TokenParser ()
lexer = Token.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

integer :: Parser Integer
integer = Token.integer lexer
