{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module ODE.Leapfrog where

-- Imports
import qualified Data.Vector.Unboxed as U
import ODE.Problem
import ODE.Utils

-- A type to represent a Leapfrog solver
data LeapfrogSolver = LeapfrogSolver
                 {
                   problem   :: PosVelProblem,
                   dt        :: Double,
                   iteration :: Int,
                   pos       :: U.Vector Double,
                   vel       :: U.Vector Double
                 }

-- For nice printing of just pos and vel
instance Show LeapfrogSolver where
  show LeapfrogSolver {..} = show (fromIntegral iteration * dt)
                               ++ " " ++ mconcat coords where
    coords = map (\x -> show x ++ " ") $ U.toList pos ++ U.toList vel

-- Smart constructor
makeLeapfrogSolver :: PosVelProblem -> Double -> Maybe LeapfrogSolver
makeLeapfrogSolver problemIn dtIn
    | dtIn <= 0.0 = Nothing
    | otherwise   = Just LeapfrogSolver
                    {
                      problem   = problemIn,
                      dt        = dtIn,
                      iteration = 0,
                      pos       = initialPos problemIn,
                      vel       = initialVel problemIn
                    }

-- Update
update :: LeapfrogSolver -> LeapfrogSolver
update LeapfrogSolver {..}
  = let
      problem'   = problem
      dt'        = dt
      iteration' = iteration + 1
      posHalf    = addMult pos vel (0.5 * dt)
      aHalf      = a posHalf
      vel'       = addMult vel aHalf dt
      pos'       = addMult posHalf vel' (0.5 * dt)
      a          = accel problem
    in
      LeapfrogSolver problem' dt' iteration' pos' vel'

