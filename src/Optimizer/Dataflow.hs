module Optimizer.Dataflow(getLiveVariables) where

import Datatypes
import Data.List


getLiveVariables :: [IR] -> [[Int]]
getLiveVariables ir =
  let live = foldr analyseLiveness [[]] ir
      shiftedLive = tail live ++ [[]] in 
  map (uncurry union) (zip live shiftedLive)


analyseLiveness :: IR -> [[Int]] -> [[Int]]
analyseLiveness (ThreeIR _ r1 r2 r3 _) (live:lives) =
  let gen = (toI r2) ++ (toI r3)
      kill = toI r1
      live' = gen `union` (live \\ kill)
  in live':live:lives

analyseLiveness (TwoIR r1 _ _) (live:lives) =
  let kill = toI r1
      live' = live \\ kill
  in live':live:lives

analyseLiveness _ lives = lives


toI :: IRItem -> [Int]
toI (R r)
  | r < 7 = []
  | otherwise = [r]

toI _ = []
