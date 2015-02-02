module Optimizer.DeadCodeElimination(optimize) where

import Datatypes
import Control.Monad.Writer
import Optimizer.Dataflow(getLiveVariables, fixedPoint)
import Data.List

optimize :: [IR] -> Writer [String] [IR]
optimize ir = do
  let prunedIR =
        fixedPoint
          (\ir ->
            let liveVariables = foldl union [] (getLiveVariables ir)
            in filter (keepInstruction liveVariables) ir)
          ir

  tell [ show (length ir - length prunedIR) ++ " statements out of " ++ show (length ir) ++ " found dead." ]
  return prunedIR

keepInstruction :: [Int] -> IR -> Bool
keepInstruction liveVars ir
  | r >= 7 = r `elem` liveVars
  | otherwise = True
  where r = case ir of
              ThreeIR _ (R target) _ _ _ -> target
              TwoIR (R target) _ _ -> target
              _ -> 0 -- To keep the other IR types
