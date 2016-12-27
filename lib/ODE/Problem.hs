module ODE.Problem where

import qualified Data.Vector.Unboxed as U

data Problem = Problem
               {
                 -- Derivative function
                 deriv :: U.Vector Double -> U.Vector Double
               }

