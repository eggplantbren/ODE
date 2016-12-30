{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module ODE.Leapfrog
  (makeLeapfrogSolver, doIterations)
  where

import qualified Data.Vector.Unboxed as U
import ODE.Problem
import ODE.Utils

-- A type to represent a solver
data LeapfrogSolver = LeapfrogSolver
                 {
                   problem   :: PosVelProblem,
                   dt        :: Double,
                   iteration :: Int,
                   pos       :: U.Vector Double,
                   vel       :: U.Vector Double
                 }

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

-- For nice printing
toString :: LeapfrogSolver -> String
toString LeapfrogSolver {..} = show (fromIntegral iteration * dt) ++ " "
                            ++ mconcat coords where
  coords = map (\x -> show x ++ " ") $ U.toList pos ++ (U.toList vel)


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

-- Do iterations
doIterations :: Int -> Int -> LeapfrogSolver
             -> IO LeapfrogSolver
doIterations n thin solver
    | n < 0     = error "Invalid input."
    | n == 0    = do
                    putStrLn $ toString solver
                    return solver
    | otherwise = do
                    -- Print info
                    if iteration solver `mod` thin == 0
                      then putStrLn $ toString solver
                    else
                      return ()

                    let !solver' = update solver
                    doIterations (n - 1) thin solver'

