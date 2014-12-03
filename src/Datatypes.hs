module Datatypes where

data Program = Program Assignment
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
                | ExpressionIdentifier Identifier 
                deriving (Eq, Show)

data Identifier = Identifier String
                deriving (Eq, Show)

data Lefthand = Lefthand Identifier
              deriving (Eq, Show)

