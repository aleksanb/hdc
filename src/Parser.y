{
module Parser(parse) where

import Tokenizer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  in  { In }

%%

Statement : in { "yolo" }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parse = parser
}
