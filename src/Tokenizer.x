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
  $digit+           { \s -> Int (read s) }
  "0x"$hexdigit+    { \s -> HexInt (read s) }
  in                { \s -> In }
  and               { \s -> And }
  or                { \s -> Or }
  "?"               { \s -> QuestionMark }
  ":"               { \s -> Colon }
  "+"               { \s -> Plus }
  "-"               { \s -> Minus }
  "*"               { \s -> Multiply }
  "<<"              { \s -> ShiftLeft }
  ">>"              { \s -> ShiftRight }
  ">>>"             { \s -> ShiftRightArithmetic }
  "<"               { \s -> LessThan }
  ">"               { \s -> GreaterThan }
  "=="              { \s -> Equal }
  "&"               { \s -> BinaryAnd }
  "|"               { \s -> BinaryOr }
  "^"               { \s -> BinaryXor }
  "="               { \s -> Assignment }
  "|="              { \s -> AssignmentOr }
  "&="              { \s -> AssignmentAnd }
  "+="              { \s -> AssignmentPlus }
  "-="              { \s -> AssignmentMinus }
  "*="              { \s -> AssignmentMultiply }
  \(                { \s -> LParen }
  \)                { \s -> RParen }

  $character+       { \s -> Identifier s }

{

data Token = In
           | And
           | Or
           | QuestionMark
           | Colon
           | Plus
           | Minus
           | Multiply
           | ShiftLeft
           | ShiftRight
           | ShiftRightArithmetic
           | LessThan
           | GreaterThan
           | Equal
           | BinaryAnd
           | BinaryOr
           | BinaryXor
           | Assignment
           | AssignmentOr
           | AssignmentAnd
           | AssignmentPlus
           | AssignmentMinus
           | AssignmentMultiply
           | LParen
           | RParen
           | Identifier String
           | Int Int
           | HexInt Int
           deriving (Eq, Show)

tokenize = alexScanTokens
