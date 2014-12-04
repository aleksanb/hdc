{
module Tokenizer (Token(..), tokenize) where
}

%wrapper "basic"

$digit = 0-9
$hexdigit = [0-9a-fA-F]
$character = [a-zA-Z]
$eol = [\n]

tokens :-
  $eol              ;
  $white+           ;
  "#".*             ;
  $digit+           { \s -> TokenInt (read s) }
  "0x"$hexdigit+    { \s -> TokenHexInt (read s) }
  in                { \s -> TokenIn }
  and               { \s -> TokenAnd }
  or                { \s -> TokenOr }
  "?"               { \s -> TokenQuestionMark }
  ":"               { \s -> TokenColon }
  "+"               { \s -> TokenPlus }
  "-"               { \s -> TokenMinus }
  "*"               { \s -> TokenMultiply }
  "<<"              { \s -> TokenShiftLeft }
  ">>"              { \s -> TokenShiftRight }
  ">>>"             { \s -> TokenShiftRightArithmetic }
  "<"               { \s -> TokenLessThan }
  ">"               { \s -> TokenGreaterThan }
  "=="              { \s -> TokenEqual }
  "&"               { \s -> TokenBinaryAnd }
  "|"               { \s -> TokenBinaryOr }
  "^"               { \s -> TokenBinaryXor }
  "="               { \s -> TokenAssignment }
  "|="              { \s -> TokenAssignmentOr }
  "&="              { \s -> TokenAssignmentAnd }
  "+="              { \s -> TokenAssignmentPlus }
  "-="              { \s -> TokenAssignmentMinus }
  "*="              { \s -> TokenAssignmentMultiply }
  \(                { \s -> TokenLParen }
  \)                { \s -> TokenRParen }

  $character+       { \s -> TokenIdentifier s }

{

data Token = TokenIn
           | TokenAnd
           | TokenOr
           | TokenQuestionMark
           | TokenColon
           | TokenPlus
           | TokenMinus
           | TokenMultiply
           | TokenShiftLeft
           | TokenShiftRight
           | TokenShiftRightArithmetic
           | TokenLessThan
           | TokenGreaterThan
           | TokenEqual
           | TokenBinaryAnd
           | TokenBinaryOr
           | TokenBinaryXor
           | TokenAssignment
           | TokenAssignmentOr
           | TokenAssignmentAnd
           | TokenAssignmentPlus
           | TokenAssignmentMinus
           | TokenAssignmentMultiply
           | TokenLParen
           | TokenRParen
           | TokenIdentifier String
           | TokenInt Int
           | TokenHexInt Int
           deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = alexScanTokens

}
