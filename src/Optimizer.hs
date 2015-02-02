module Optimizer(optimize) where

import qualified Optimizer.Constants as Constants
import qualified Optimizer.RegisterAllocation as RegisterAllocation
import qualified Optimizer.CopyPropagation as CopyPropagation
import qualified Optimizer.DeadCodeElimination as DeadCodeElimination

import Datatypes
import Control.Monad.Writer

optimize :: [IR] -> Writer [String] [IR]
optimize ir = do
  let constantsOptimized = Constants.optimize ir
  foldM
    (\ir f -> f ir)
    constantsOptimized
    [CopyPropagation.optimize, DeadCodeElimination.optimize, RegisterAllocation.optimize]
