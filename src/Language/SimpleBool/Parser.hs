module Language.SimpleBool.Parser (termP) where

import Language.Base.Parser (getInfo)
import Language.SimpleBool.Lexer
import Language.SimpleBool.Syntax
import Text.Parsec (Parsec, chainl1, try, (<|>))
import Text.Parsec.String (Parser)

tyBoolP :: Parser Ty
tyBoolP = symbol "Bool" >> return TyBool

tyArrP :: Parser Ty
tyArrP = try $ do
  d <- tyBoolP
  _ <- symbol "->"
  r <- tyBoolP
  return $ TyArr d r

tyP :: Parser Ty
tyP = tyArrP <|> tyBoolP

tmTrueP, tmFalseP :: Parser TermN
tmTrueP = reserved "true" >> TmTrueN <$> getInfo
tmFalseP = reserved "false" >> TmFalseN <$> getInfo

tmIfP :: Parser TermN -> Parser TermN
tmIfP p =
  TmIfN
    <$> getInfo
    <*> (reserved "if" >> p)
    <*> (reserved "then" >> p)
    <*> (reserved "else" >> p)

absP :: Parser TermN -> Parser TermN
absP bodyP = do
  _ <- reservedOp "\\"
  v <- identifier
  _ <- symbol ":"
  t <- tyP
  _ <- reservedOp "."
  _ <- spaces
  b <- bodyP
  info <- getInfo
  return $ TmAbsN info v t b

metaVarP :: Parser TermN
metaVarP = do
  _ <- reservedOp "$"
  v <- identifier
  info <- getInfo
  return $ TmMetaVarN info v

varP :: Parser TermN
varP = do
  v <- identifier
  info <- getInfo
  return $ TmVarN info v

nonAppP :: Parser TermN
nonAppP =
  parens termP
    <|> tmIfP termP
    <|> tmTrueP
    <|> tmFalseP
    <|> absP termP
    <|> metaVarP
    <|> varP

appP :: Parsec String () (TermN -> TermN -> TermN)
appP = do
  _ <- spaces
  info <- getInfo
  return $ TmAppN info

termP :: Parser TermN
termP = nonAppP `chainl1` appP
