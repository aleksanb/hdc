module Optimizer.RegisterAllocation(optimize) where

import Datatypes
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Optimizer.Dataflow(getLiveVariables)

data CG =
  CG {
    registers :: [Int],
    tempMap :: Map.Map Int Int,
    permaMap :: Map.Map Int Int
  }
  deriving (Show)

optimize :: [IR] -> Writer [String] [IR]
optimize ir = do
  let liveVariables = getLiveVariables ir
      registersUsed = maximum $ map length liveVariables
      CG _ _ registerMappings = execState
        (mapM_ allocateRegisters liveVariables)
        (CG [7..] Map.empty Map.empty)

  tell [ "Program mapped using " ++ show registersUsed ++ " of 9 available registers" ]
  return $ map (rewriteRegisters registerMappings) ir

--------------------------
-- Rewrite instructions --
--------------------------

rewriteRegisters :: Map.Map Int Int -> IR -> IR
rewriteRegisters permaMap (ThreeIR op r1 r2 r3 m) =
  let r1' = getPermaReg r1 permaMap
      r2' = getPermaReg r2 permaMap
      r3' = getPermaReg r3 permaMap
  in ThreeIR op r1' r2' r3' m

rewriteRegisters permaMap (TwoIR r1 r2 m) =
  let r1' = getPermaReg r1 permaMap
      r2' = getPermaReg r2 permaMap
  in TwoIR r1' r2' m

rewriteRegisters _ other = other


getPermaReg :: IRItem -> Map.Map Int Int -> IRItem
getPermaReg (R reg) permaMap
  | reg < 7 = R reg
  | otherwise =
    case Map.lookup reg permaMap of
      Just val -> R val
      _ -> R 0

getPermaReg other _ = other

-------------------------
-- Register allocation --
-------------------------

allocateRegisters :: [Int] -> State CG ()
allocateRegisters liveVars = do
  CG regs tempMap permaMap <- get
  let boundVars = Map.keys tempMap
      freedVars = boundVars \\ liveVars
      generatedVars = liveVars \\ boundVars

  mapM_ freeVar freedVars
  mapM_ generateVar generatedVars


generateVar :: Int -> State CG ()
generateVar liveVar
  | liveVar < 7 = do return ()
  | otherwise = do
    CG (reg:regs) tempMap permaMap <- get
    put (CG
          regs
          (Map.insert liveVar reg tempMap)
          permaMap)


freeVar :: Int -> State CG ()
freeVar liveVar
  | liveVar < 7 = do return ()
  | otherwise = do
    CG regs tempMap permaMap <- get
    let val = fromJust (Map.lookup liveVar tempMap)
    put (CG
          (val:regs)
          (Map.delete liveVar tempMap)
          (Map.insert liveVar val permaMap))
