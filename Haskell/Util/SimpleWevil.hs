module Haskell.Util.SimpleWevil where


import           Debug.Trace
import           Haskell.Phases
import           Haskell.Util.Mixed
import           Haskell.Util.Norm
import           Haskell.Util.RK4

traceThis:: (Show a) => a -> a
traceThis x = trace (show x) x

preventNegs::([Double]->[Double]) -> [Double]->[Double]
preventNegs f [m,c] = [dm_,dc_]
  where
    dm_ = if m <= 0 then max 0 dm else dm
    dc_ = if c <= 0 then max 0 dc else dc
    [dm,dc] = f [m,c]

adjust::[Double] -> Phase -> Phase
adjust v (ODE (con,tsys)) = ODE (con, (\ a -> preventNegs (\b -> zipWith (-) (tsys a b) v)))

wevilModel:: Double -> Double -> [Phase]
wevilModel wevilM wevilC = [(adjust [wevilM,wevilC] preSurface), (adjust [wevilM,wevilC] postSurface) ,winter]

carbToCarb:: Double -> Double -> Double -> Double -> Double -> Double
carbToCarb tol step wevilM wevilC c =  fst . (!!1) . snd . last $  multiPhase tol step  (wevilModel wevilM wevilC ) (0,[(0,0),(c,0)])

equalib:: Double -> Double -> Double -> Double -> Double -> Double
equalib eps tol step wevilM wevilC = result
  where
    func = carbToCarb tol step wevilM wevilC :: Double -> Double
    chain = iterate func 1000 :: [Double]
    result = numLim eps chain :: Double

createMap::Double -> Double -> Double -> Double -> Double -> Double -> ([[Double]],[[Double]],[[Double]])
createMap eps tol xstep xstop ystep ystop = unzip3 $ map unzip3 $ map (map (carbToSeasonPeak tol 1)) [[ (x , y ,trace (show (x,y)) $ equalib eps tol 1 x y) | x <- [0,xstep..xstop] ] | y <- [0,ystep..ystop] ]

carbToSeasonPeak::Double -> Double -> (Double,Double,Double)->(Double,Double,Double)
carbToSeasonPeak tol step (m,c,lc) = (m,c,peak)
  where
    model = wevilModel m c ::[Phase]
    frames = multiPhase tol step model (0,[(0,0),(lc,0)]) ::[Frame]
    ms = map (fst . head . snd) frames :: [Double]
    peak = maximum ms :: Double
