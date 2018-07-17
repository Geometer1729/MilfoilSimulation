module Haskell.Phases (winter,postSurface,preSurface) where

import           Haskell.Params
import           Haskell.Util.Mixed
import           Haskell.Util.RK4
--just for the type names

winter::Phase
winter = DYN cold

cold::Frame->Frame
cold f = (0,[(1,0),(0.6*c,0)])
  where
    c = fst ( snd f !! 1 )

postSurface::Phase
postSurface = ODE ( (\f -> (fst f) > 100) , clearWatter)

preSurface::Phase
preSurface = ODE ( \f -> (fst f) > 100 || (fst . head . snd) f > ps * d , uniformBiomass )

clearWatter::TSystem
clearWatter _ [0,_] = [0,0]
clearWatter t [m,c] = [dm,dc]
  where
    dc = dm*(0.06/0.94)
    dm = (* 0.94) $ (mewt / km) + log ( ( k1 + i0 ) / ( k1 + ird ) ) - m*( (lambdafunc . temp) t + delta )
    mewt = mew0 * thetag**( temp t - tb )
    i0 = iradiance t
    ird = i0 * exp(-(kwt*d+ km*m))

uniformBiomass::TSystem
uniformBiomass _ [0,_] = [0,0]
uniformBiomass t [m,c] = [dm,dc]
  where
    dm = mewt *m/(kwt * height + km *m) * log(( k1 + i0 )/(k1 + ird)) -m*( (lambdafunc . temp) t + delta ) -dc
    mewt = mew0 * thetag**( temp t - tb )
    dc = -mew1 * c * (msat/(1+msat))
    msat = m / chm
    height = min d m / ps
    i0 = iradiance t
    ird = i0 * exp(-(kwt*d+ km*m))

sinusoid :: Double -> Double -> Double -> Double
sinusoid amp phase input = amp * sin((pi*(input+phase))/360)**2

temp :: Double -> Double
temp = sinusoid tempAmp tempPhase

iradiance:: Double -> Double
iradiance = sinusoid iradianceAmp iradiancePhase

lambdafunc ::  Double -> Double
lambdafunc temp = lambda0 * ( thetar ** (temp - tb))
