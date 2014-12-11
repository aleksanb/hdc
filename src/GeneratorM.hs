module GeneratorM(generate, getGeneratedCode) where

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

getGeneratedCode = generatedCode


generate :: Program -> CodeGenState
generate program =
  let cgs = CGS [7..100] Map.empty []
      CGS r v c = execState (generateProgram program) cgs
  in CGS r v $ reverse c


generateProgram :: Program -> State CodeGenState ()
generateProgram (Program statements) = do
  mapM generateStatement statements
  return ()


generateStatement :: Statement -> State CodeGenState ()
generateStatement (AssignmentStatement (Assignment item _ e1)) = do
  reg1 <- generateExpression e1
  lefthand <- registerForItem item

  emitInstruction $ ThreeIR Plus lefthand (R 0) reg1 False


generateStatement (BuiltinStatement builtinStatement) = do
  let instruction =
        case builtinStatement of
          LoadStatement -> LoadIR
          storeStatement -> StoreIR

  emitInstruction instruction


generateExpression :: Expression -> State CodeGenState IRItem
generateExpression (BinaryExpression op e1 e2) = do
  reg1 <- generateExpression e1
  reg2 <- generateExpression e2

  targetReg <- getRegister
  emitInstruction $ ThreeIR op targetReg reg1 reg2 False

  return targetReg


generateExpression (TernaryExpression e1 e2 e3) = do
  reg1 <- generateExpression e1
  reg2 <- generateExpression e2
  reg3 <- generateExpression e3

  targetReg <- getRegister
  emitInstruction $ ThreeIR Plus (R 6) (R 0) reg1 False
  emitInstruction $ ThreeIR Plus targetReg (R 0) reg2 False
  emitInstruction $ ThreeIR Plus targetReg (R 0) reg3 True

  return targetReg


generateExpression (ExpressionItem item) = do
  CGS (register:registers) variables generatedCode <- get
  case item of
    Variable name -> return $ R $ fromJust $ Map.lookup name variables
    Register name -> return $ fromRegister name
    Immediate int -> do
      reg <- getRegister
      emitInstruction $ TwoIR reg (I int) False
      return reg

    Constant constant -> do
      reg <- getRegister
      emitInstruction $ TwoIR reg (C constant) False
      return reg

-----------------------------------------------
-- Move the functions below to separate file --
-----------------------------------------------

getRegister :: State CodeGenState IRItem
getRegister = do
  state popRegister
  where popRegister (CGS (r:rs) v c) = ((R r), CGS rs v c)


registerForItem :: Item -> State CodeGenState IRItem
registerForItem (Register register) = do
  return $ fromRegister register

registerForItem (Variable variable) = do
  CGS (register:registers) variables code <- get
  put (CGS registers (Map.insert variable register variables) code)
  return $ R register


emitInstruction :: IR -> State CodeGenState ()
emitInstruction instruction = do
  state appendInstruction
  where appendInstruction (CGS rs v is) = ((), CGS rs v (instruction:is))


fromRegister :: String -> IRItem
fromRegister register =
  let mapping = Map.fromList [("zero", 0), ("id_high", 1), ("id_low", 2), ("address_high", 3), ("address_low", 4), ("data", 5), ("mask", 6)]
  in R $ fromJust (Map.lookup register mapping)
