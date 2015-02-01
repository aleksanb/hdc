module Optimizer.CopyPropagation(optimize) where

import Datatypes
import Control.Monad.State
import Control.Monad.Writer
import Optimizer.Dataflow(getLiveVariables, fixedPoint)
import qualified Data.Map as Map

type CopyMappings = Map.Map Int Int

optimize :: [IR] -> Writer [String] [IR]
optimize ir = do
  let liveVariables = getLiveVariables ir
      copyMappings = execState (mapM_ collectCopyStatement ir) Map.empty
      optimizedIR =
        fixedPoint
          (\ir -> evalState (mapM propagateCopyStatements ir) copyMappings)
          ir

  tell [ "Found "
          ++ show (Map.size copyMappings)
          ++ " copy statement(s): "
          ++ show copyMappings ]
  return optimizedIR


propagateCopyStatements :: IR -> State CopyMappings IR
propagateCopyStatements (ThreeIR op r1 r2 r3 m) = do
  r2' <- getMapping r2
  r3' <- getMapping r3
  return (ThreeIR op r1 r2' r3' m)

propagateCopyStatements (TwoIR r1 r2 m) = do
  r2' <- getMapping r2
  return (TwoIR r1 r2' m)

propagateCopyStatements others = do return others


collectCopyStatement :: IR -> State CopyMappings ()
collectCopyStatement (ThreeIR Plus (R r1) (R r2) (R r3) _)
  | r2 == 0 = putMapping r1 r3
  | r3 == 0 = putMapping r1 r2

collectCopyStatement others = do return ()


getMapping :: IRItem -> State CopyMappings IRItem
getMapping (R reg) = do
  mapping <- get
  case Map.lookup reg mapping of
    Just val -> do return (R val)
    _ -> do return (R reg)

getMapping others = do return others


putMapping :: Int -> Int -> State CopyMappings () 
putMapping from to
  | from < 7 = do return ()
  | otherwise = do
    mapping <- get
    put $ Map.insert from to mapping
