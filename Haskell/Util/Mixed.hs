module Haskell.Util.Mixed (multiPhase,Phase(ODE,DYN))where

import           Haskell.Params
import           Haskell.Util.RK4

data Phase = ODE (Con,TSystem) | DYN (Frame -> Frame)

run::Double -> Double -> Phase -> Frame -> [Frame]
run _ _ (DYN p) f              = [p f]
run tol step (ODE (con,sys)) f = simulate tol step con sys f

multiPhase::Double -> Double -> [Phase] -> Frame -> [Frame]
multiPhase _ _ [] _ = []
multiPhase tol step (p:ps) f =  thisSim ++ multiPhase tol step ps (last thisSim)
  where
    thisSim = run tol step p f
