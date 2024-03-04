module Language.Intro.Parser (expression, topLevel) where

import Data.Functor.Identity (Identity)
import Language.Intro.Lexer
import Language.Intro.Syntax (Expression (..))
import Text.Parsec (choice, eof, (<?>))
import Text.Parsec.Expr (Assoc (..), Operator (..))
import Text.Parsec.Expr qualified as Expr
import Text.Parsec.String (Parser)

binaryOperator ::
  String ->
  (a -> a -> a) ->
  Assoc ->
  Operator String () Identity a
binaryOperator name f assoc = Infix (f <$ reservedOp name) assoc

metaVar :: Parser String
metaVar = symbol "$" *> identifier

expression :: Parser Expression
expression = Expr.buildExpressionParser table term <?> "expression"
  where
    table =
      [ [binaryOperator "*" Mul AssocLeft],
        [binaryOperator "+" Add AssocLeft]
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
