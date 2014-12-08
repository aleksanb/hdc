module Beautifier(beautify) where
import Datatypes


beautify :: Program -> Program
beautify (Program statements) =
  Program (map beautifyStatement statements)


beautifyStatement :: Statement -> Statement
beautifyStatement (AssignmentStatement (Assignment lefthand assignmentOperator oldExpression)) =
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
  where expression = beautifyExpression oldExpression

beautifyStatement other = other


beautifyExpression :: Expression -> Expression
beautifyExpression (IsInList lefthand [expression]) =
  (BinaryExpression
    EqualTo
    lefthand
    (beautifyExpression expression))


beautifyExpression (IsInList lefthand (expression:expressions)) =
  BinaryExpression
    Or
    (beautifyExpression (IsInList lefthand expressions))
    (BinaryExpression
      EqualTo
      lefthand
      (beautifyExpression expression))

-- Ensure recursion over nested expressions
beautifyExpression (BinaryExpression op e1 e2) =
  case op of
    GreaterThan ->
      BinaryExpression
        LessThan
        (beautifyExpression e2)
        (beautifyExpression e1)
    _ ->
      BinaryExpression
        op
        (beautifyExpression e1)
        (beautifyExpression e2)

beautifyExpression (TernaryExpression e1 e2 e3) =
  TernaryExpression
    (beautifyExpression e1)
    (beautifyExpression e2)
    (beautifyExpression e3)

beautifyExpression others = others
