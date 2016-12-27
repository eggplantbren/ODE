module Main where

import qualified Data.Vector.Unboxed as U
import ODE.Problem
import ODE.RungeKutta4


-- Damped Pendulum 
pendulumDeriv :: U.Vector Double -> U.Vector Double
pendulumDeriv state = U.fromList [v, - sin x - 0.01*v] where
                        x   = state U.! 0
                        v   = state U.! 1

-- Pendulum problem
pendulumProblem :: Problem
pendulumProblem = Problem (U.fromList [1.0, 0.0]) pendulumDeriv

-- Main action
main :: IO ()
main = do
  -- Run parameters
  let dt     = 0.01
      tFinal = 100.0
      thin   = 10
      steps  = floor $ tFinal / dt

  -- Construct the solver
  let solver = case (makeRK4Solver pendulumProblem dt) of
        Nothing -> error "Failed to create solver."
        Just s  -> s

  -- Run the solver
  _ <- doIterations steps thin solver

  return ()

