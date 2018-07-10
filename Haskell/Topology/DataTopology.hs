module Haskell.Topology.DataTopology where

import           Haskell.Topology.Complex
import           Haskell.MainCarbsimlib
import           System.Environment
import           System.IO

--- generate data
{-
makeGrowthSurface:: Double -> Double -> [[Double]]
makeGrowthSurface m t = fmap ptgrowth (make2Sameple [0..m]  [0..t])


ptgrowth:: [Double] -> [Double]
ptgrowth (m:t:[]) = [m,dm,tmp]
  where
    (dm,dc) = cwgrowth  m 0 t
    tmp = ((temp . season) norwoodEnv) t
-}


make2Sameple:: [Double] -> [Double] -> [[Double]]
make2Sameple as bs = [ [a,b] | a<-as , b<-bs]
