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
                        | AssignmentOr
                        | AssignmentAnd
                        | AssignmentPlus
                        | AssignmentMinus
                        | AssignmentMultiply
                        deriving (Eq, Show)

data Expression = And Expression Expression
                | Or Expression Expression
                | BinaryAnd Expression Expression
                | BinaryOr Expression Expression
                | BinaryXor Expression Expression
                | IsInList Expression List
                | Plus Expression Expression
                | Minus Expression Expression
                | Multiply Expression Expression
                | LessThan Expression Expression
                | GreaterThan Expression Expression
                | EqualTo Expression Expression
                | ShiftLeft Expression Expression
                | ShiftRight Expression Expression
                | ShiftRightArithmetic Expression Expression
                | TernaryExpression Expression Expression Expression
                | EnclosedExpression Expression
                | ExpressionIdentifier Lefthand
                | ExpressionImmediate Number
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

data List = List ElementList
          deriving (Eq, Show)

data ElementList = ElementList ElementList ListItem
                 | ElementListItem ListItem
                 deriving (Eq, Show)

data ListItem = ItemLefthand Lefthand
              | ItemConstant Number
              | ItemImmediate Number
              deriving (Eq, Show)

data Number = DecimalInt Int
            | HexInt Int
            deriving (Eq, Show)
