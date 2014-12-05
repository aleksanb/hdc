module Beautifier(beautify) where
import Datatypes


beautify :: Program -> Program
beautify (Program statements) =
  Program (map beautifyStatement statements)


beautifyStatement :: Statement -> Statement
beautifyStatement (AssignmentStatement (Assignment lefthand assignmentOperator oldExpression)) =
  let expression = beautifyExpression oldExpression in
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
            (ExpressionItem lefthand)
            expression))


beautifyExpression :: Expression -> Expression
beautifyExpression (IsInList expression [item]) =
  (BinaryExpression
    EqualTo
    expression
    (ExpressionItem item))


beautifyExpression (IsInList expression (item:items)) =
  BinaryExpression
    Or
    (beautifyExpression (IsInList expression items))
    (BinaryExpression
      EqualTo
      expression
      (ExpressionItem item))


beautifyExpression others = others
