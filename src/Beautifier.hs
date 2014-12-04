module Beautifier(beautify) where
import Datatypes


beautify :: Program -> Program
beautify (Program statements) =
  Program (map beautifyStatement statements)


beautifyStatement :: Statement -> Statement
beautifyStatement (AssignmentStatement (Assignment lefthand assignmentOperator expression)) =
  case assignmentOperator of
    AssignmentStraightUp ->
      AssignmentStatement
        (Assignment
          lefthand
          assignmentOperator
          expression)

    AssignmentBinaryOp binaryOperation -> -- Handle all other operators
      AssignmentStatement
        (Assignment
          lefthand
          AssignmentStraightUp
          (BinaryExpression
            binaryOperation
            (ExpressionIdentifier lefthand)
            expression))


beautifyStatement statement = statement
