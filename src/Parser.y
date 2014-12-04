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
  "$"               { TokenDollar }
  "("               { TokenLParen }
  ")"               { TokenRParen }
  "["               { TokenLBracket }
  "]"               { TokenRBracket }
  ","               { TokenComma }
  load_statement    { TokenLoad }
  store_statement   { TokenStore }
  identifier        { TokenIdentifier $$ }

%%

program : statement_list { Program $1 }

statement_list : statement_list statement { $2 : $1 }
               | statement { [$1] }

statement : assignment { AssignmentStatement $1 }
          | builtin { BuiltinStatement $1 }

builtin : load_statement { LoadStatement }
        | store_statement { StoreStatement }

assignment : lefthand assignment_operator expression { Assignment $1 $2 $3 }

assignment_operator : "="    { AssignmentStraightUp }
                    | "|="   { AssignmentBinaryOp BitwiseOr }
                    | "&="   { AssignmentBinaryOp BitwiseAnd }
                    | "+="   { AssignmentBinaryOp Plus }
                    | "-="   { AssignmentBinaryOp Minus }
                    | "*="   { AssignmentBinaryOp Multiply }

lefthand : variable { LefthandVariable $1 }
         | register { LefthandRegister $1 }

expression : expression and expression   { BinaryExpression And $1 $3 }
           | expression or expression    { BinaryExpression Or $1 $3 }
           | expression "&" expression   { BinaryExpression BitwiseAnd $1 $3 }
           | expression "|" expression   { BinaryExpression BitwiseOr $1 $3 }
           | expression "^" expression   { BinaryExpression BitwiseXor $1 $3 }
           | expression "+" expression   { BinaryExpression Plus $1 $3 }
           | expression "-" expression   { BinaryExpression Minus $1 $3 }
           | expression "*" expression   { BinaryExpression Multiply $1 $3 }
           | expression "<" expression   { BinaryExpression LessThan $1 $3 }
           | expression ">" expression   { BinaryExpression GreaterThan $1 $3 }
           | expression "==" expression  { BinaryExpression EqualTo $1 $3 }
           | expression "<<" expression  { BinaryExpression ShiftLeft $1 $3 }
           | expression ">>" expression  { BinaryExpression ShiftRightArithmetic $1 $3 }
           | expression ">>>" expression { BinaryExpression ShiftRight $1 $3 }
           | expression "?" expression ":" expression { TernaryExpression $1 $3 $5 }
           | expression in "[" list "]"  { IsInList $1 $4 }
           | "(" expression ")"          { $2 }
           | lefthand                    { ExpressionIdentifier $1 }
           | immediate                   { ExpressionImmediate $1 }


list : list "," list_item { $3 : $1 }
     | list_item { [$1] }

list_item : lefthand { ItemLefthand $1 }
          | constant { ItemConstant $1 }
          | immediate { ItemImmediate $1 }

variable : identifier { Variable $1 }

register : "$" identifier { Register $2 }

constant : int { DecimalInt $1 }

immediate : int { DecimalInt $1 }
          | hexint { HexInt $1 }

{

parseError :: [Token] -> a
parseError s = error ("Parse error" ++ show s)

parse :: String -> Program
parse = parser . tokenize

}
