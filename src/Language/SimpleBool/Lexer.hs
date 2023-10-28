module Language.SimpleBool.Lexer
  ( parens,
    reserved,
    reservedOp,
    identifier,
    spaces,
    symbol,
  )
where

import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

reservedNames :: [String]
reservedNames = ["if", "then", "else", "true", "false"]

langDef :: Token.LanguageDef ()
langDef =
  Token.LanguageDef
    { Token.commentStart = "{-",
      Token.commentEnd = "-}",
      Token.commentLine = "--",
      Token.nestedComments = True,
      Token.identStart = letter,
      Token.identLetter = alphaNum <|> oneOf "_'",
      Token.opStart = Token.opLetter langDef,
      Token.opLetter = oneOf "!$%&|*+-/:<=>?@^_~",
      Token.reservedNames = reservedNames,
      Token.reservedOpNames = [],
      Token.caseSensitive = True
    }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

spaces :: Parser ()
spaces = Token.whiteSpace lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer
