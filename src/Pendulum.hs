module Main where

-- Imports
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import ODE.Leapfrog
import ODE.Problem
import ODE.Utils

-- Pendulum accel function
pendulumAccel :: U.Vector Double -> U.Vector Double
pendulumAccel position = U.fromList [a] where
  a = -(sin x)
  x = position U.! 0

-- Pendulum problem
pendulumProblem :: PosVelProblem
pendulumProblem = PosVelProblem (U.fromList [1.0]) (U.fromList [0.0])
                                    pendulumAccel

-- Main action
main :: IO ()
main = do
  -- Run parameters
  let timeStep = 0.001
      tFinal   = 100.0
      thin     = 10
      steps    = floor $ tFinal / timeStep

  -- Construct the solver
  let solver = fromMaybe (error "Failed to create solver.")
                                (makeLeapfrogSolver pendulumProblem timeStep)

  -- Run the solver
  _ <- doIterations steps thin solver ODE.Leapfrog.update iteration

  return ()

