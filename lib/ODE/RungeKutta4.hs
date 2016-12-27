module ODE.RungeKutta4
  (makeRK4Solver)
  where

import qualified Data.Vector.Unboxed as U
import ODE.Problem

-- A type to represent a solver
data RK4Solver = RK4Solver
                 {
                   problem      :: Problem,
                   dt           :: Double,
                   iteration    :: Int,
                   currentState :: U.Vector Double
                 }

-- Smart constructor
makeRK4Solver :: Problem -> Double -> Maybe RK4Solver
makeRK4Solver problemIn dtIn
    | dtIn <= 0.0 = Nothing
    | otherwise   = Just RK4Solver
                    {
                      problem      = problemIn,
                      dt           = dtIn,
                      iteration    = 0,
                      currentState = initialConditions problemIn
                    }

-- Update
update :: RK4Solver -> RK4Solver
update solver = solver


