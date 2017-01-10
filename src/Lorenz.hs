module Main where

-- O,[prts
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import ODE.Problem
import ODE.RungeKutta4
import ODE.Utils

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
lorenzProblem :: FirstOrderProblem
lorenzProblem = FirstOrderProblem (U.fromList [1.0, 0.0, 0.0]) lorenzDeriv

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
                                (makeRK4Solver lorenzProblem timeStep)

  -- Run the solver
  _ <- doIterations steps thin solver ODE.RungeKutta4.update iteration

  return ()

