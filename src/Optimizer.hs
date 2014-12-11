module Optimizer(optimize) where

import qualified Optimizer.Constants as Constants
import Datatypes

optimize :: [IR] -> [IR]
optimize ir = Constants.optimize ir
