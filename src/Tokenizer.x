{
module Tokenizer (Token(..), tokenize) where
}

%wrapper "basic"

$digit = 0-9
$character = [a-zA-Z]
$eol = [\n]

tokens :-
  $eol              ;
  $white+           ;
  "#".*             ;
  $digit+           ;
  in                { \s -> In }

{

data Token = In
           deriving (Eq, Show)

tokenize = alexScanTokens
