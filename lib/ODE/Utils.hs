module ODE.Utils where

import qualified Data.Vector.Unboxed as U

-- Vector operations
-- Input x, y, scalar c
-- Output x + c*y
addMult :: U.Vector Double -> U.Vector Double -> Double
        -> U.Vector Double
addMult x y c   = U.zipWith (+) x cy where
  cy = U.map (* c) y

