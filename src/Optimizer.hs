module Optimizer(optimize) where

import qualified Optimizer.Constants as Constants
import qualified Optimizer.RegisterAllocation as RegisterAllocation
import Datatypes

optimize :: [IR] -> [IR]
optimize ir = (RegisterAllocation.optimize
 . Constants.optimize) ir
