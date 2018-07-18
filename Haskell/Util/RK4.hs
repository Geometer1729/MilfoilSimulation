{-# LANGUAGE FlexibleInstances #-}
module Haskell.Util.RK4 (smartRK4Step,rK4Step,simulate,getPt,Pos,Frame,Con,TSystem) where
import           Haskell.Util.Norm

type Pos = (Double , Double) -- state then change

type Frame = (Double,[Pos]) -- time and variable positions
type Con = (Frame -> Bool)

instance Normed Frame where
  norm f1 f2 = norm (getPt f1) (getPt f2)

class NaNable a where
  hasNaN:: a -> Bool

instance NaNable Double where
  hasNaN = isNaN

instance (NaNable a,NaNable b) => NaNable (a,b) where
  hasNaN (x,y) = hasNaN x || hasNaN y

instance NaNable a => NaNable [a] where
  hasNaN [] = False
  hasNaN xs = or $ map hasNaN xs

getPt::Frame -> [Double]
getPt = map fst . snd

type System = [Double] -> [Double] -- ode System

type TSystem = Double -> System -- time dependent System

rK4Step :: Double -> TSystem -> Frame -> Frame
rK4Step step sys (t,ic) = if hasNaN nf then error (show (t,ic)) else nf
  where
    iv = map fst ic :: [Double]
    roll = nextK sys t step iv
    gen = fmap roll [0,1/2,1/2,1] :: [[Double] -> [Double]]
    ks = psedoIterate gen iv :: [[Double]]
    fs = weightedSumLists [1, 1/6 ,1/3,1/3,1/6] ks
    wd = zip fs (sys (t+step) fs)
    nf = (t+step , wd)

nextK:: TSystem -> Double -> Double -> [Double] -> Double ->  [Double] -> [Double]
nextK sys t step iv weight xs = map (*step) (sys ( t + step * weight ) ( zipWith (+) iv (map (*weight) xs) ) ) :: [Double]

psedoIterate:: [a->a] -> a -> [a]
psedoIterate (f:fs) x = x : psedoIterate fs (f x)
psedoIterate [] x     = [x]

weightedSumLists::[Double]->[[Double]]->[Double]
weightedSumLists ws xs = foldr (zipWith (+)) [0,0..] $ zipWith (\ x -> map (*x)) ws xs

smartRK4Step::Double -> Double -> TSystem -> Frame -> Frame
smartRK4Step tol step sys f = if err > tol && (step/4 /= 0) then smartRK4Step tol (step/2) sys f else byHStep
  where
    err = norm dir byHStep
    dir =  rK4Step step sys f :: Frame
    byHStep = hStep . hStep $ f :: Frame
    hStep = rK4Step (step/2) sys :: Frame -> Frame

simulate:: Double -> Double ->  Con -> TSystem -> Frame -> [Frame]
simulate tol step con sys f = takeWhile (not . con) $ iterate (smartRK4Step tol step sys) f
