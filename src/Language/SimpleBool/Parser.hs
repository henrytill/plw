{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.SimpleBool.Parser (termP) where

import           Language.Base.Parser       (infoFrom)
import           Language.SimpleBool.Lexer
import           Language.SimpleBool.Syntax
import           Text.Parsec                (Parsec, chainl1, getPosition, try,
                                             (<|>))
import           Text.Parsec.String         (Parser)


tyBoolP :: Parser Ty
tyBoolP = symbol "Bool" >> return TyBool

tyArrP :: Parser Ty
tyArrP = try $ do
  dom <- tyBoolP
  _   <- symbol "->"
  rng <- tyBoolP
  return $ TyArr dom rng

tyP :: Parser Ty
tyP = tyArrP <|> tyBoolP

tmTrueP, tmFalseP :: Parser Term
tmTrueP  = reserved "true"  >> TmTrue  <$> (fmap infoFrom getPosition)
tmFalseP = reserved "false" >> TmFalse <$> (fmap infoFrom getPosition)

tmIfP :: Parser Term -> Parser Term
tmIfP p =
  TmIf <$> (fmap infoFrom getPosition)
       <*> (reserved "if"   >> p)
       <*> (reserved "then" >> p)
       <*> (reserved "else" >> p)

absP :: Parser Term -> Parser Term
absP bodyP = do
  _   <- reservedOp "\\"
  v   <- identifier
  _   <- symbol ":"
  ty  <- tyP
  _   <- reservedOp "."
  _   <- spaces
  b   <- bodyP
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v ty b

metaVarP :: Parser Term
metaVarP = do
  _   <- reservedOp "$"
  v   <- identifier
  pos <- getPosition
  return $ TmMetaVar (infoFrom pos) v

varP :: Parser Term
varP = do
  v   <- identifier
  pos <- getPosition
  return $ TmVar (infoFrom pos) v

nonAppP :: Parser Term
nonAppP = parens termP
      <|> tmIfP termP
      <|> tmTrueP
      <|> tmFalseP
      <|> absP termP
      <|> metaVarP
      <|> varP

appP :: Parsec String () (Term -> Term -> Term)
appP = do
  _   <- spaces
  pos <- getPosition
  return $ TmApp (infoFrom pos)

termP :: Parser Term
termP = nonAppP `chainl1` appP
