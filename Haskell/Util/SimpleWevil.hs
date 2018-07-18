module Haskell.Util.SimpleWevil where

import           Haskell.Phases
import           Haskell.Util.Mixed
import           Haskell.Util.Norm
import           Haskell.Util.RK4


subVec::[Double] -> Phase -> Phase
subVec v (ODE (con,tsys)) = ODE (con, (\ a b -> zipWith (-) (tsys a b) v))

wevilModel:: Double -> Double -> [Phase]
wevilModel wevilM wevilC = [(subVec [wevilM,wevilC] preSurface), (subVec [wevilM,wevilC] postSurface) ,winter]

carbToCarb:: Double -> Double -> Double -> Double -> Double -> Double
carbToCarb tol step wevilM wevilC c =  fst . (!!1) . snd . last $ ( multiPhase tol step  (wevilModel wevilM wevilC ) (0,[(1,0),(c,0)]) )

equalib:: Double -> Double -> Double -> Double -> Double -> Double
equalib eps tol step wevilM wevilC = result
  where
    func = carbToCarb tol step wevilM wevilC :: Double -> Double
    chain = iterate func 1 :: [Double]
    result = numLim eps chain :: Double

createMap::Double -> Double -> Double -> Double -> [(Double,Double)]
createMap eps tol step stop = [ x , equalib eps tol 1 x 0) | x <- [0,step..stop]]
