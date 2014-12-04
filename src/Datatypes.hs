module Datatypes where

data Program = Program [Statement]
             deriving (Eq, Show)

data Statement = AssignmentStatement Assignment
               | BuiltinStatement Builtin
               deriving (Eq, Show)

data Builtin = LoadStatement
             | StoreStatement
             deriving (Eq, Show)

data Assignment = Assignment Lefthand AssignmentOperator Expression
                deriving (Eq, Show)

data AssignmentOperator = AssignmentStraightUp
                        | AssignmentBinaryOp BinaryOp
                        deriving (Eq, Show)

data Expression = BinaryExpression BinaryOp Expression Expression
                | TernaryExpression Expression Expression Expression
                | IsInList Expression [ListItem]
                | ExpressionIdentifier Lefthand
                | ExpressionImmediate Number
                deriving (Eq, Show)

data BinaryOp = And
              | Or
              | BitwiseAnd
              | BitwiseOr
              | BitwiseXor
              | Plus
              | Minus
              | Multiply
              | LessThan
              | GreaterThan
              | EqualTo
              | ShiftLeft
              | ShiftRight
              | ShiftRightArithmetic
              deriving (Eq, Show)

data ListItem = ItemLefthand Lefthand
              | ItemConstant Number
              | ItemImmediate Number
              deriving (Eq, Show)

data Identifier = Identifier String
                deriving (Eq, Show)

data Lefthand = LefthandVariable Variable
              | LefthandRegister Register
              deriving (Eq, Show)

data Variable = Variable String
              deriving (Eq, Show)

data Register = Register String
              deriving (Eq, Show)

data Number = DecimalInt Int
            | HexInt Int
            deriving (Eq, Show)
