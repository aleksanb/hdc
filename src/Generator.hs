module Generator(generate) where
import Datatypes
import Text.Printf
import Data.Maybe
import qualified Data.Map as Map

data CodeGenState =
  CGS {
    availableRegisters :: [Int],
    variables :: (Map.Map String Int),
    generatedCode :: [IR]
  }
  deriving (Show)


generateProgram :: CodeGenState -> Program -> CodeGenState
generateProgram cgs (Program statements) =
  let (CGS registers variables generatedCode) = foldl (\cgs statement -> generateStatement cgs statement)  cgs statements
  in CGS registers variables $ reverse generatedCode


generateStatement :: CodeGenState -> Statement -> CodeGenState
generateStatement cgs (AssignmentStatement (Assignment lefthand assignmentOperator righthand)) =
  let ((CGS (freeRegister:rest) variables generatedCode), reg1) = generateExpression cgs righthand

      targetRegister = case lefthand of
        Register register ->
          fromRegister register
        Variable variable ->
          if Map.member variable variables then
            fromJust (Map.lookup variable variables)
          else
            freeRegister

      newvariables = case lefthand of
        Register _ ->
          variables
        Variable variable ->
          Map.insert variable targetRegister variables

      assignment = generateIR Plus targetRegister 0 reg1 

  in CGS rest newvariables (assignment : generatedCode)

generateStatement (CGS registers variables generatedCode) (BuiltinStatement builtin) =
  case builtin of
    LoadStatement ->
      CGS registers variables $ LoadIR:generatedCode
    StoreStatement ->
      CGS registers variables $ StoreIR:generatedCode

generateExpression :: CodeGenState -> Expression -> (CodeGenState, Int)
generateExpression cgs (BinaryExpression op e1 e2) =
  let (cgs1, reg1) = generateExpression cgs e1
      ((CGS (freeRegister:rest) variables generatedCode), reg2) = generateExpression cgs1 e2
      assignment = generateIR op freeRegister reg1 reg2
  in (CGS rest variables (assignment : generatedCode), freeRegister)

generateExpression cgs@(CGS (freeRegister:registers) variables generatedCode) (ExpressionItem item) =
  case item of
    Variable name ->
      (cgs, fromJust (Map.lookup name variables))
    Register name ->
      (cgs, fromRegister name)
    DecimalInt decimalInt ->
      (CGS registers variables (LoadImmediateIR freeRegister decimalInt:generatedCode), freeRegister )
    HexInt hexInt ->
      (CGS registers variables (LoadImmediateIR freeRegister hexInt:generatedCode), freeRegister)
    Constant constant ->
      (CGS registers variables (LoadConstantIR freeRegister constant:generatedCode), freeRegister)


fromRegister :: String -> Int
fromRegister register =
  let mapping = Map.fromList [("zero", 0), ("id_high", 1), ("id_low", 2), ("address_high", 3), ("address_low", 4), ("data", 5), ("mask", 6)]
  in fromJust (Map.lookup register mapping)

generateIR :: BinaryOp -> Int -> Int -> Int -> IR
generateIR op lhs operand1 operand2 =
  ThreeIR op lhs operand1 operand2
  -- Commented block here provides assembly output endpoint
  --let mapping = Map.fromList [(And, "and"), (Or, "or"), (BitwiseAnd, "and"), (BitwiseOr, "or"), (BitwiseXor, "xor"), (Plus, "add"), (Minus, "sub"), (Multiply, "mul"), (LessThan, "slt"), (EqualTo, "seq")]
  --in (fromJust (Map.lookup op mapping)) ++ " $" ++ show lhs ++ " $" ++ show operand1 ++ " $" ++ show operand2 ++ "\n"

-- Commented mainblock useful for testing things separately
--main = do
--  let cgs = CGS [7..15] (Map.fromList [("a", 10)]) []
--      variable = generateExpression cgs (ExpressionItem (Variable "a"))
--      simpleStatement = AssignmentStatement
--        (Assignment
--          (Variable "b")
--          AssignmentStraightUp
--          (BinaryExpression
--            Plus
--            (ExpressionItem (Register "data"))
--            (BinaryExpression
--              Plus
--              (ExpressionItem (Register "data"))
--              (ExpressionItem (Register "data")))))
--      simpleProgram = Program [simpleStatement]
--
--  putStrLn ""
--  putStrLn $ show variable
--  putStrLn ""
--  putStrLn $ show $ generateStatement cgs simpleStatement
--  putStrLn ""
--  putStrLn $ show $ generateProgram cgs simpleProgram
--  putStrLn ""

--let cgs = CGS [7..15] (Map.fromList [("a", 10)]) []
-- generateExpression cgs (ExpressionItem (Variable "a"))
-- generateExpression cgs (BinaryExpression Plus (ExpressionItem (Variable "a")) (ExpressionItem (Register "data")))
-- generateIR Plus 10 10 10

-- let assignment = AssignmentStatement (Assignment (Variable "a") AssignmentStraightUp (BinaryExpression Plus (ExpressionItem (Variable "data")) (BinaryExpression Plus (ExpressionItem (Variable "data")) (ExpressionItem (Variable "data")))))
--generateStatement cgs assignment

generate = generateProgram (CGS [7..100] Map.empty [])
