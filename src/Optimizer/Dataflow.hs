module Optimizer.Dataflow(getLiveVariables) where

import Datatypes
import Data.List
import Debug.Trace


getLiveVariables :: [IR] -> [[Int]]
getLiveVariables ir =
  map (uncurry union) (zip live shiftedLive)
  where live = foldr analyseLiveness [[]] ir
        shiftedLive = tail live ++ [[]]

analyseLiveness :: IR -> [[Int]] -> [[Int]]
analyseLiveness (ThreeIR _ r1 r2 r3 masked) (live:lives)
  | masked = live:live:lives
  | otherwise =
    live':live:lives
  where gen = (toI r2) `union` (toI r3)
        kill = toI r1
        live' = gen `union` (live \\ kill)

analyseLiveness (TwoIR r1 _ masked) (live:lives)
  | masked = live:live:lives
  | otherwise = live':live:lives
  where live' = live \\ (toI r1)

analyseLiveness _ lives = lives


toI :: IRItem -> [Int]
toI (R r)
  | r < 7 = []
  | otherwise = [r]

toI _ = []
