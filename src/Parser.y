{
module Parser(parse) where

import Tokenizer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  int               { TokenInt $$ }
  hexint            { TokenHexInt $$ }
  in                { TokenIn }
  and               { TokenAnd }
  or                { TokenOr }
  "?"               { TokenQuestionMark }
  ":"               { TokenColon }
  "+"               { TokenPlus }
  "-"               { TokenMinus }
  "*"               { TokenMultiply }
  "<<"              { TokenShiftLeft }
  ">>"              { TokenShiftRight }
  ">>>"             { TokenShiftRightArithmetic }
  "<"               { TokenLessThan }
  ">"               { TokenGreaterThan }
  "=="              { TokenEqual }
  "&"               { TokenBinaryAnd }
  "|"               { TokenBinaryOr }
  "^"               { TokenBinaryXor }
  "="               { TokenAssignment }
  "|="              { TokenAssignmentOr }
  "&="              { TokenAssignmentAnd }
  "+="              { TokenAssignmentPlus }
  "-="              { TokenAssignmentMinus }
  "*="              { TokenAssignmentMultiply }
  "("                { TokenLParen }
  ")"                { TokenRParen }
  identifier        { TokenIdentifier $$ }

%%

program : assignment       { Program $1 }

assignment : lefthand assignment_operator expression { Assignment $1 $2 $3 }

assignment_operator : "="    { AssignmentStraightUp }
                    | "|="   { AssignmentOr }
                    | "&="   { AssignmentAnd }
                    | "+="   { AssignmentPlus }
                    | "-="   { AssignmentMinus }
                    | "*="   { AssignmentMultiply }

lefthand : variable { $1 }

expression : expression and expression { And $1 $3 }
           | variable                { ExpressionIdentifier $1 }


variable : identifier { Identifier $1 }

{

data Program = Program Assignment
             deriving (Eq, Show)

data Assignment = Assignment Identifier AssignmentOperator Expression
                deriving (Eq, Show)

data AssignmentOperator = AssignmentStraightUp
                        | AssignmentOr
                        | AssignmentAnd
                        | AssignmentPlus
                        | AssignmentMinus
                        | AssignmentMultiply
                        deriving (Eq, Show)

data Expression = And Expression Expression
                | ExpressionIdentifier Identifier 
                deriving (Eq, Show)

data Identifier = Identifier String
                deriving (Eq, Show)


parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Program
parse = parser . tokenize

}
