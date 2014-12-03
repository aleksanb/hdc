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

statement : assignment       { $1 }


assignment : lefthand assignment_operator expression { $1 $2 $3 }

assignment_operator : "="    { AssignmentStraightUp }
                    | "|="   { AssignmentOr }
                    | "&="   { AssignmentAnd }
                    | "+="   { AssignmentPlus }
                    | "-="   { AssignmentMinus }
                    | "*="   { AssignmentMultiply }

lefthand : identifier { $1 }

expression : expression and expression { $1 And $3 }
           | identifier                { $1 }

{

data Statement = Assignment
               deriving (Eq, Show)

data Assignment = Lefthand AssignmentOperator Expression
                deriving (Eq, Show)

data Identifier = Identifier String

data Lefthand = Identifier
              | SomethingElse

data AssignmentOperator = AssignmentStraightUp
                        | AssignmentOr
                        | AssignmentAnd
                        | AssignmentPlus
                        | AssignmentMinus
                        | AssignmentMultiply
                        deriving (Eq, Show)

data ExpressionOp = And
                  deriving (Eq, Show)

data Expression = Expression ExpressionOp Expression
                | Identifier
                deriving (Eq, Show)


parseError :: [Token] -> a
parseError _ = error "Parse error"

parse = parser
}
