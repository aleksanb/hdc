module Optimizer(optimize) where

import Datatypes
import qualified Data.Map as Map
import Data.Maybe

type Constants = Map.Map Int Int

optimize :: [IR] -> [IR]
optimize ir = map fst (map f ir) where f = propagateConstant (Map.empty)

propagateConstant :: Constants -> IR -> (IR, Constants)
propagateConstant constants ir@(LoadImmediateIR target immediate _) =
  (ir, Map.insert target immediate constants)

propagateConstant constants ir@(RRR op r1 r2 r3 mask) =
  if Map.member r3 constants then
    (RRI op r1 r2 (fromJust (Map.lookup r3 constants)) mask, Map.delete r1 constants)
  else
    (ir, Map.delete r1 constants)
