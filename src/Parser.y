{

module Parser(parse) where

import Tokenizer
import Datatypes

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
  "("               { TokenLParen }
  ")"               { TokenRParen }
  "["               { TokenLBracket }
  "]"               { TokenRBracket }
  ","               { TokenComma }
  identifier        { TokenIdentifier $$ }

%%

program : statement_list { Program $1 }

statement_list : statement_list statement { StatementList $1 $2 }
               | statement { StatementListItem $1 }

statement : assignment { AssignmentStatement $1 }

assignment : lefthand assignment_operator expression { Assignment $1 $2 $3 }

assignment_operator : "="    { AssignmentStraightUp }
                    | "|="   { AssignmentOr }
                    | "&="   { AssignmentAnd }
                    | "+="   { AssignmentPlus }
                    | "-="   { AssignmentMinus }
                    | "*="   { AssignmentMultiply }

lefthand : variable { Lefthand $1 }

expression : expression and expression   { And $1 $3 }
           | expression or expression    { Or $1 $3 }
           | expression in list          { IsInList $1 $3 }
           | expression "&" expression   { BinaryAnd $1 $3 }
           | expression "|" expression   { BinaryOr $1 $3 }
           | expression "^" expression   { BinaryXor $1 $3 }
           | expression "+" expression   { Plus $1 $3 }
           | expression "-" expression   { Minus $1 $3 }
           | expression "*" expression   { Multiply $1 $3 }
           | expression "<" expression   { LessThan $1 $3 }
           | expression ">" expression   { GreaterThan $1 $3 }
           | expression "==" expression  { EqualTo $1 $3 }
           | expression "<<" expression  { ShiftLeft $1 $3 }
           | expression ">>" expression  { ShiftRightArithmetic $1 $3 }
           | expression ">>>" expression { ShiftRight $1 $3 }
           | expression "?" expression ":" expression { TernaryExpression $1 $3 $5 }
           | "(" expression ")"          { EnclosedExpression $2 }
           | variable                    { ExpressionIdentifier $1 }
           | immediate                   { ExpressionImmediate $1 }

list : "[" element_list "]" { List $2 }

element_list : element_list "," list_item { ElementList $1 $3 }
             | list_item { ElementListItem $1 }

list_item : lefthand { ItemLefthand $1 }
          | constant { ItemConstant $1 }
          | immediate { ItemImmediate $1 }

variable : identifier { Identifier $1 }

constant : int { DecimalInt $1 }

immediate : int { DecimalInt $1 }
          | hexint { HexInt $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Program
parse = parser . tokenize

}
