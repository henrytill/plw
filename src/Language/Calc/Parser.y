-- -*- mode: prog; tab-width: 2; -*-
{
module Language.Calc.Parser where

import Language.Calc.Lexer
import Language.Calc.Syntax
}

%name calc
%tokentype { Token }
%error { parseError }

%token
  var   { TokenVar _ $$ }
  const { TokenConst _ $$ }
  '+'   { TokenPlus _ }
  '-'   { TokenMinus _ }
  '*'   { TokenTimes _ }
  '^'   { TokenExp _ }
  '('   { TokenLParen _ }
  ')'   { TokenRParen _ }

%left '+' '-'
%left '*'
%right '^'
%nonassoc UMINUS

%%

Exp : var                  { Var $1 }
    | const                { Const $1 }
    | Exp '^' Exp          { Exp $1 $3 }
    | Exp '*' Exp          { Mul $1 $3 }
    | Exp '+' Exp          { Add $1 $3 }
    | Exp '-' Exp          { Sub $1 $3 }
    | '-' Exp %prec UMINUS { Neg $2 }
    | '(' Exp ')'          { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
