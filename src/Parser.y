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
  constants         { TokenConstants }
  identifier        { TokenIdentifier $$ }

%right "?" ":"
%left or
%left and
%left in
%left "|"
%left "^"
%left "&"
%left "=="
%left "<" ">"
%left "<<" ">>" ">>>"
%left "+" "-"
%left "*"
%nonassoc UMINUS

%%

program : statement_list { Program (reverse $1) }

statement_list : statement_list statement { $2 : $1 }
               | statement { [$1] }

statement : assignment { AssignmentStatement $1 }
          | builtin { BuiltinStatement $1 }

builtin : load_statement { LoadStatement }
        | store_statement { StoreStatement }

assignment : variable assignment_operator expression { Assignment $1 $2 $3 }
           | register assignment_operator expression { Assignment $1 $2 $3 }

assignment_operator : "="    { AssignmentStraightUp }
                    | "|="   { AssignmentBinaryOp BitwiseOr }
                    | "&="   { AssignmentBinaryOp BitwiseAnd }
                    | "+="   { AssignmentBinaryOp Plus }
                    | "-="   { AssignmentBinaryOp Minus }
                    | "*="   { AssignmentBinaryOp Multiply }

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
           | "-" expression %prec UMINUS { BinaryExpression Minus (ExpressionItem (Immediate 0)) $2 }
           | expression in "[" list "]"  { IsInList $1 $4 }
           | "(" expression ")"          { $2 }
           | items                       { ExpressionItem $1 }


list : list "," expression { $3 : $1 }
     | expression { [$1] }

items : variable  { $1 }
      | register  { $1 }
      | immediate { $1 }
      | constant  { $1 }

variable : identifier { Variable $1 }

register : "$" identifier { Register $2 }

constant : "$" constants "[" int "]" { Constant $4 }

immediate : int { Immediate $1 }
          | hexint { Immediate $1 }

{

parseError :: [Token] -> a
parseError s = error ("Parse error" ++ show s)

parse :: String -> Program
parse = parser . tokenize

}
