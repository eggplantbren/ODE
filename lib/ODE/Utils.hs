{-# LANGUAGE BangPatterns #-}

module ODE.Utils where

-- Imports
import Control.Monad (when)
import qualified Data.Vector.Unboxed as U

-- Vector operations
-- Input x, y, scalar c
-- Output x + c*y
addMult :: U.Vector Double -> U.Vector Double -> Double
        -> U.Vector Double
addMult x y c   = U.zipWith (+) x cy where
  cy = U.map (* c) y

-- IO action to do many iterations on a solver
doIterations :: Show a =>
                Int           -- Number of iterations
             -> Int           -- Thinning
             -> a             -- A solver of some sort
             -> (a -> a)      -- A function to update the solver once.
             -> (a -> Int)    -- A function to get the iteration number.
             -> IO a          -- An IO action updating the solver.
doIterations n thin !solver updater getIteration
    | n < 0     = error "Invalid input."
    | n == 0    = do
                    print solver
                    return solver
    | otherwise = do
                    -- Print info
                    when (getIteration solver `mod` thin == 0) (print solver)

                    let solver' = updater solver
                    doIterations (n - 1) thin solver' updater getIteration

