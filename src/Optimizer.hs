module Optimizer(optimize) where

import qualified Optimizer.Constants as Constants
import qualified Optimizer.RegisterAllocation as RegisterAllocation
import Datatypes
import Control.Monad.Writer

optimize :: [IR] -> Writer [String] [IR]
optimize ir = do
  let constantsOptimized = Constants.optimize ir
  RegisterAllocation.optimize constantsOptimized
