module Language.Intro.Parser (expression, topLevel) where

import Data.Functor.Identity (Identity)
import Language.Intro.Lexer
import Language.Intro.Syntax (Expression (..))
import Text.Parsec (choice, eof, (<?>))
import Text.Parsec.Expr (Assoc (..), Operator (..))
import Text.Parsec.Expr qualified as Expr
import Text.Parsec.String (Parser)

prefixOperator ::
  String ->
  (a -> a) ->
  Operator String () Identity a
prefixOperator name f = Prefix (f <$ reservedOp name)

binaryOperator ::
  String ->
  (a -> a -> a) ->
  Assoc ->
  Operator String () Identity a
binaryOperator name f = Infix (f <$ reservedOp name)

metaVar :: Parser String
metaVar = symbol "$" *> identifier

expression :: Parser Expression
expression = Expr.buildExpressionParser table term <?> "expression"
  where
    table =
      [ [prefixOperator "-" Neg],
        [binaryOperator "^" Exp AssocRight, binaryOperator "*" Mul AssocLeft],
        [binaryOperator "+" Add AssocLeft, binaryOperator "-" Sub AssocLeft]
      ]
    term =
      choice
        [ parens expression,
          Var <$> identifier,
          Const <$> integer,
          MetaVar <$> metaVar
        ]
        <?> "simple expression"

topLevel :: Parser a -> Parser a
topLevel p = whiteSpace *> p <* eof
