module Optimizer.Dataflow(getLiveVariables, fixedPoint) where

import Datatypes
import Data.List
import Debug.Trace


getLiveVariables :: [IR] -> [[Int]]
getLiveVariables ir =
  map (uncurry union) (zip live shiftedLive)
  where live = foldr analyseLiveness [[]] ir
        shiftedLive = tail live ++ [[]]

analyseLiveness :: IR -> [[Int]] -> [[Int]]
analyseLiveness (ThreeIR _ r1 r2 r3 masked) rest@(liveOut:lives)
  | masked = liveInMasked:rest
  | otherwise = liveIn:rest
  where gen = (toI r2) `union` (toI r3)
        liveInMasked = gen `union` liveOut -- Masked instructions only generate liveness
        liveIn = gen `union` (liveOut \\ (toI r1))

analyseLiveness (TwoIR r1 _ masked) rest@(liveOut:lives)
  | masked = liveOut:rest -- Masked instructions don't kill variables
  | otherwise = liveIn:rest
  where liveIn = liveOut \\ (toI r1)

analyseLiveness _ lives = lives


toI :: IRItem -> [Int]
toI (R r)
  | r < 7 = []
  | otherwise = [r]

toI _ = []


fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x
  | x == x' = x
  | otherwise = fixedPoint f x'
  where x' = f x
