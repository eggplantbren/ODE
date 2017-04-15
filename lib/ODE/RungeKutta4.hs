{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module ODE.RungeKutta4 where

-- Imports
import qualified Data.Vector.Unboxed as U
import ODE.Problem
import ODE.Utils

-- A type to represent a Runge Kutta 4 solver
data RK4Solver = RK4Solver
                 {
                   problem      :: FirstOrderProblem,
                   dt           :: Double,
                   iteration    :: Int,
                   currentState :: U.Vector Double
                 }

-- Smart constructor
makeRK4Solver :: FirstOrderProblem -> Double -> Maybe RK4Solver
makeRK4Solver problemIn dtIn
    | dtIn <= 0.0 = Nothing
    | otherwise   = Just RK4Solver
                    {
                      problem      = problemIn,
                      dt           = dtIn,
                      iteration    = 0,
                      currentState = initialConditions problemIn
                    }

-- For nice printing of just currentState
instance Show RK4Solver where
  show RK4Solver {..} = show (fromIntegral iteration * dt) ++ " "
                            ++ mconcat coords where
    coords = map (\x -> show x ++ " ") $ U.toList currentState

-- Update
update :: RK4Solver -> RK4Solver
update RK4Solver {..}
  = let
      problem'       = problem
      dt'            = dt
      iteration'     = iteration + 1
      currentState'  = U.zipWith5 func currentState f1 f2 f3 f4
      f1             = d currentState
      f2             = d $ addMult currentState f1 (0.5 * dt)
      f3             = d $ addMult currentState f2 (0.5 * dt)
      f4             = d $ addMult currentState f3 dt
      d              = deriv problem
      dtOverSix      = dt / 6.0
      func x y z u v = x + dtOverSix * (y + 2.0*z + 2.0*u + v)
    in
      RK4Solver problem' dt' iteration' currentState'

