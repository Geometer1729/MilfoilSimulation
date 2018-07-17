module Haskell.Util.SimpleWevil where
import           Haskell.Util.Norm

import           Haskell.Phases
import           Haskell.Util.Mixed
import           Haskell.Util.RK4

addVec::[Double] -> Phase -> Phase
addVec v (ODE (con,tsys)) = ODE (con, (\ a b -> zipWith (+) v  (tsys a b)))

wevilModel:: Double -> Double -> [Phase]
wevilModel m c = [preSurface, (addVec [m,c] postSurface) ,winter]

carbToCarb:: Double -> Double -> Double -> Double -> Double -> Double
carbToCarb tol step wevilM wevilC c =  fst . (!!1) . snd . last $ ( multiPhase tol step  (wevilModel wevilM wevilC ) (0,[(1,0),(c,0)]) )

equalib:: Double -> Double -> Double -> Double -> Double -> Double
equalib eps tol step wevilM wevilC = result
  where
    func = carbToCarb tol step wevilM wevilC :: Double -> Double
    chain = iterate func 1 :: [Double]
    result = numLim eps chain :: Double
