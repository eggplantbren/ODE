module Main where

import qualified Data.Vector.Unboxed as U
import ODE.Problem
import ODE.RungeKutta4


-- Lorenz system 
lorenzDeriv :: U.Vector Double -> U.Vector Double
lorenzDeriv state = U.fromList [xDot, yDot, zDot] where
                        x = state U.! 0
                        y = state U.! 1
                        z = state U.! 2
                        xDot = sigma * (y - x)
                        yDot = x * (rho - z) - y
                        zDot = x * y - beta * z
                        sigma = 10.0
                        beta  = 8 / 3
                        rho   = 28.0

-- Lorenz problem
lorenzProblem :: Problem
lorenzProblem = Problem (U.fromList [1.0, 0.0, 0.0]) lorenzDeriv

-- Main action
main :: IO ()
main = do
  -- Run parameters
  let dt     = 0.001
      tFinal = 100.0
      thin   = 10
      steps  = floor $ tFinal / dt

  -- Construct the solver
  let solver = case (makeRK4Solver lorenzProblem dt) of
        Nothing -> error "Failed to create solver."
        Just s  -> s

  -- Run the solver
  _ <- doIterations steps thin solver

  return ()

