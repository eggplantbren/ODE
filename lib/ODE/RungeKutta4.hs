module ODE.RungeKutta4 where

import ODE.Problem

data RK4Solver = RK4Solver
                 {
                   dt      :: Double,
                   problem :: Problem
                 }

