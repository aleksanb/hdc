module Datatypes where

data Program = Program [Statement]
             deriving (Eq, Show)

data Statement = AssignmentStatement Assignment
               | BuiltinStatement Builtin
               deriving (Eq, Show)

data Builtin = LoadStatement
             | StoreStatement
             deriving (Eq, Show)

data Assignment = Assignment Item AssignmentOperator Expression
                deriving (Eq, Show)

data AssignmentOperator = AssignmentStraightUp
                        | AssignmentBinaryOp BinaryOp
                        deriving (Eq, Show)

data Expression = BinaryExpression BinaryOp Expression Expression
                | TernaryExpression Expression Expression Expression
                | IsInList Expression [Expression]
                | ExpressionItem Item
                deriving (Eq, Show)

data BinaryOp = BitwiseAnd
              | BitwiseOr
              | BitwiseXor
              | Plus
              | Minus
              | Multiply
              | Power
              | LessThan
              | GreaterThan
              | EqualTo
              | ShiftLeft
              | ShiftRight
              | ShiftRightArithmetic
              deriving (Eq, Show)

data Item = Variable String
          | Register String
          | Constant Int
          | Immediate Int
          deriving (Eq, Show)

data IR = ThreeIR BinaryOp IRItem IRItem IRItem Bool
        | TwoIR IRItem IRItem Bool
        | LoadIR
        | StoreIR
        deriving (Eq, Show)

data IRItem = R Int
            | C Int
            | I Int
            deriving (Eq, Show)
