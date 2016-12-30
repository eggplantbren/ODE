module Main where

import qualified Data.Vector.Unboxed as U
import ODE.Problem
import ODE.Leapfrog
import Data.Maybe (fromMaybe)

-- Pendulum accel function
pendulumAccel :: U.Vector Double -> U.Vector Double
pendulumAccel pos = U.fromList [a] where
  a = -(sin x)
  x = pos U.! 0

-- Pendulum problem
pendulumProblem :: PosVelProblem
pendulumProblem = PosVelProblem (U.fromList [1.0]) (U.fromList [0.0])
                                    pendulumAccel

-- Main action
main :: IO ()
main = do
  -- Run parameters
  let dt     = 0.001
      tFinal = 100.0
      thin   = 10
      steps  = floor $ tFinal / dt

  -- Construct the solver
  let solver = fromMaybe (error "Failed to create solver.")
                                (makeLeapfrogSolver pendulumProblem dt)

  -- Run the solver
  _ <- doIterations steps thin solver

  return ()

