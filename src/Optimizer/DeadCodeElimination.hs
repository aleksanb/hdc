module Optimizer.DeadCodeElimination(optimize) where

import Datatypes
import Control.Monad.Writer
import Optimizer.Dataflow(getLiveVariables, fixedPoint)
import Data.List

optimize :: [IR] -> Writer [String] [IR]
optimize ir = do
  let prunedIR = fixedPoint (\ir -> let liveVariables = foldl union [] (getLiveVariables ir) in filter (keepInstruction liveVariables) ir) ir

  tell [ show (length ir - length prunedIR) ++ " statements out of " ++ show (length ir) ++ " found dead." ]
  return prunedIR


keepInstruction :: [Int] -> IR -> Bool
keepInstruction liveVars (ThreeIR _ (R target) _ _ _)
  | target >= 7 = target `elem` liveVars

keepInstruction liveVars (TwoIR (R target) _ _)
  | target >= 7 = target `elem` liveVars

keepInstruction liveVars others = True
