module ODE.Problem where

import qualified Data.Vector.Unboxed as U

data FirstOrderProblem = FirstOrderProblem -- 'Set of first order ODEs' style
                         {
                           -- Initial conditions
                           initialConditions :: U.Vector Double,

                           -- Derivative function
                           deriv :: U.Vector Double -> U.Vector Double
                         }

data PosVelProblem = PosVelProblem -- A problem with positions and velocities
                     {
                       -- Initial conditions
                       initialPos :: U.Vector Double,
                       initialVel :: U.Vector Double,

                       -- Derivative function (only depends on positions)
                       accel :: U.Vector Double -> U.Vector Double
                     }

