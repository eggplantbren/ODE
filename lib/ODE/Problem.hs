module ODE.Problem where

import qualified Data.Vector.Unboxed as U

data Problem = Problem
               {
                 -- Initial conditions
                 initialConditions :: U.Vector Double,

                 -- Derivative function
                 deriv             :: U.Vector Double -> U.Vector Double
               }

