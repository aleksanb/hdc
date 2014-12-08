module GeneratorM(generate) where

import Datatypes
import Text.Printf
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as Map


data CodeGenState =
  CGS {
    availableRegisters :: [Int],
    variables :: (Map.Map String Int),
    generatedCode :: [IR]
  }
  deriving (Show)


generate :: Program -> CodeGenState
generate program =
  let cgs = CGS [7..100] Map.empty []
      CGS r v c = snd $ runState (generateProgram program) cgs
  in CGS r v $ reverse c


generateProgram :: Program -> State CodeGenState Int
generateProgram (Program statements) = do
  mapM generateStatement statements
  return 1


generateStatement :: Statement -> State CodeGenState Int
generateStatement (AssignmentStatement (Assignment item _ e1)) = do
  reg1 <- generateExpression e1
  lefthand <- registerForItem item

  emitAssignmentWithTarget Plus lefthand 0 reg1 False


generateStatement (BuiltinStatement builtinStatement) = do
  CGS registers variables generatedCode <- get
  let statement =
        case builtinStatement of
          LoadStatement -> LoadIR
          storeStatement -> StoreIR

  put (CGS registers variables (statement:generatedCode))
  return 1


generateExpression :: Expression -> State CodeGenState Int
generateExpression (BinaryExpression op e1 e2) = do
  reg1 <- generateExpression e1
  reg2 <- generateExpression e2

  emitAssignment op reg1 reg2 False


generateExpression (TernaryExpression e1 e2 e3) = do
  reg1 <- generateExpression e1
  reg2 <- generateExpression e2
  reg3 <- generateExpression e3

  targetReg <- getFreeRegister

  emitAssignmentWithTarget Plus 6 0 reg1 False
  emitAssignmentWithTarget Plus targetReg 0 reg2 False
  emitAssignmentWithTarget Plus targetReg 0 reg3 True


generateExpression (ExpressionItem item) = do
  CGS (register:registers) variables generatedCode <- get
  case item of
    Variable name -> return (fromJust (Map.lookup name variables))
    Register name -> return $ fromRegister name
    DecimalInt int -> emitImmediate int False
    HexInt int -> emitImmediate int False
    Constant constant -> emitConstant constant False

-----------------------------------------------
-- Move the functions below to separate file --
-----------------------------------------------

getFreeRegister :: State CodeGenState Int
getFreeRegister = do
  CGS (register:registers) variables generatedCode <- get
  put (CGS registers variables generatedCode)
  return register


registerForItem :: Item -> State CodeGenState Int
registerForItem (Register register) = do
  return $ fromRegister register


registerForItem (Variable variable) = do
  CGS (register:registers) variables code <- get
  case Map.member variable variables of
    True -> do
      return (fromJust (Map.lookup variable variables))
    _ -> do
      put (CGS registers (Map.insert variable register variables) code)
      return register


emitAssignment :: BinaryOp -> Int -> Int -> Bool -> State CodeGenState Int
emitAssignment op reg1 reg2 masked = do
  CGS (register:registers) variables generatedCode <- get
  let instruction = ThreeIR op register reg1 reg2 masked
  put (CGS registers variables (instruction:generatedCode))
  return register


emitAssignmentWithTarget :: BinaryOp -> Int -> Int -> Int -> Bool -> State CodeGenState Int
emitAssignmentWithTarget op reg1 reg2 reg3 masked = do
  CGS registers variables generatedCode <- get
  let instruction = ThreeIR op reg1 reg2 reg3 masked
  put (CGS registers variables (instruction:generatedCode))
  return reg1


emitImmediate :: Int -> Bool -> State CodeGenState Int
emitImmediate immediate masked = do
  CGS (register:registers) variables generatedCode <- get
  let instruction = LoadImmediateIR register immediate masked
  put (CGS registers variables (instruction:generatedCode))
  return register


emitConstant :: Int -> Bool -> State CodeGenState Int
emitConstant constant masked = do
  CGS (register:registers) variables generatedCode <- get
  let instruction = LoadConstantIR register constant masked
  put (CGS registers variables (instruction:generatedCode))
  return register


fromRegister :: String -> Int
fromRegister register =
  let mapping = Map.fromList [("zero", 0), ("id_high", 1), ("id_low", 2), ("address_high", 3), ("address_low", 4), ("data", 5), ("mask", 6)]
  in fromJust (Map.lookup register mapping)
