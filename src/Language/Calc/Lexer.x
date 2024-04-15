-- -*- mode: prog; tab-width: 2; -*-
{
module Language.Calc.Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                          ;
  $alpha [$alpha $digit \_ \']*    { \p s -> TokenVar p s }
  \$ $alpha [$alpha $digit \_ \']* { \p s -> TokenMetaVar p (drop 1 s) }
  $digit+                          { \p s -> TokenConst p (read s) }
  \+                               { \p _ -> TokenPlus p }
  \-                               { \p _ -> TokenMinus p }
  \*                               { \p _ -> TokenTimes p }
  \^                               { \p _ -> TokenExp p }
  \(                               { \p _ -> TokenLParen p }
  \)                               { \p _ -> TokenRParen p }

{
data Token
  = TokenVar AlexPosn String
  | TokenMetaVar AlexPosn String
  | TokenConst AlexPosn Integer
  | TokenPlus AlexPosn
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenExp AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  deriving (Eq, Show)

scanTokens :: AlexPosn -> String -> [Token]
scanTokens startPos s = go (startPos, '\n', [], s)
  where
    go a@(pos, _, _, str) =
      case alexScan a 0 of
        AlexEOF -> []
        AlexError ((AlexPn _ line column), _, _, _) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
        AlexSkip  a' _ -> go a'
        AlexToken a' len act -> act pos (take len str) : go a'

lex :: AlexPosn -> String -> [Token]
lex = scanTokens
}
