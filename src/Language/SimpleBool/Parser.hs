module Language.SimpleBool.Parser (termP) where

import Language.Base (infoFrom)
import Language.SimpleBool.Lexer
import Language.SimpleBool.Syntax
import Text.Parsec (Parsec, chainl1, getPosition, try, (<|>))
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
tmTrueP = reserved "true" >> TmTrueN <$> (fmap infoFrom getPosition)
tmFalseP = reserved "false" >> TmFalseN <$> (fmap infoFrom getPosition)

tmIfP :: Parser TermN -> Parser TermN
tmIfP p =
  TmIfN
    <$> (fmap infoFrom getPosition)
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
  pos <- getPosition
  return $ TmAbsN (infoFrom pos) v t b

metaVarP :: Parser TermN
metaVarP = do
  _ <- reservedOp "$"
  v <- identifier
  pos <- getPosition
  return $ TmMetaVarN (infoFrom pos) v

varP :: Parser TermN
varP = do
  v <- identifier
  pos <- getPosition
  return $ TmVarN (infoFrom pos) v

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
  pos <- getPosition
  return $ TmAppN (infoFrom pos)

termP :: Parser TermN
termP = nonAppP `chainl1` appP
