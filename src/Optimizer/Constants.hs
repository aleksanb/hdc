module Optimizer.Constants(optimize) where

import Datatypes
import Data.Maybe
import Data.Bits
import Control.Monad.State
import qualified Data.Map as Map

type Constants = Map.Map Int Int


optimize :: [IR] -> [IR]
optimize ir = map cleanZeros $ fixedPoint ir


cleanZeros :: IR -> IR
cleanZeros ir@(ThreeIR op rd (I i) rt m)
  | i == 0 = ThreeIR op rd (R 0) rt m
cleanZeros ir@(ThreeIR op rd rs (I i) m)
  | i == 0 = ThreeIR op rd rs (R 0) m
cleanZeros other = other


fixedPoint ir
  | ir == ir' = ir
  | otherwise = optimize ir'
  where ir' = evalState (propagateConstants ir) (Map.fromList [(0, 0)])

propagateConstants :: [IR] -> State Constants [IR]
propagateConstants ir = do
  mapM (propagateConstant) ir


propagateConstant :: IR -> State Constants IR
propagateConstant ir@(TwoIR (R r1) (I i1) m) = do
  constants <- get
  put $ Map.insert r1 i1 constants
  return ir

propagateConstant (ThreeIR op (R r1) (I i1) (I i2) m) = do
  let newImmediate = operatorFor op i1 i2
      newIR = TwoIR (R r1) (I newImmediate) m

  constants <- get
  put $ Map.insert r1 newImmediate constants
  return newIR

propagateConstant original@(ThreeIR op (R r1) r2 r3 mask) = do
  r2' <- getFor r2
  r3' <- getFor r3
  let patched = ThreeIR op (R r1) r2' r3' mask

  constants <- get

  case (r2', r3') of
    (I i1, I i2) -> propagateConstant patched
    (I i1, R _)
      | op == Plus -> do
        put $ Map.delete r1 constants
        return patched
    (R _, I i1)
      | op `elem` [Plus, ShiftLeft, ShiftRight, ShiftRightArithmetic] -> do
        put $ Map.delete r1 constants
        return patched
    _ -> do
        put $ Map.delete r1 constants
        return original


propagateConstant other = do return other


getFor :: IRItem -> State Constants IRItem
getFor (R reg) = do
  constants <- get
  return (case Map.lookup reg constants of
            Just constant -> I constant
            _ -> R reg)

getFor other = do return other

operatorFor :: BinaryOp -> Int -> Int -> Int
operatorFor op =
  case op of
    BitwiseAnd -> (.&.)
    BitwiseOr -> (.|.)
    BitwiseXor -> xor
    Plus -> (+)
    Minus -> (-)
    Multiply -> (*)
    ShiftLeft -> shiftL
    ShiftRight -> rotateR
    ShiftRightArithmetic -> shiftR
    EqualTo -> \a b -> fromEnum  (a == b)
    LessThan -> \a b -> fromEnum (a < b)
    GreaterThan -> \a b -> fromEnum (a >= b)
