-- module PhaseLib where
import           Data.List
import           Dynamify
import           FuncToIO
import           MainCarbsimlib
import           System.Environment

type PhaseEnd = Frame -> Bool
data Phase = Dif (Enviornment,GrowthFunc,PhaseEnd) | Dynamic (Frame -> Frame)

wholeSim :: [Phase] -> Double -> Double -> Double -> Double -> [Simulation]
wholeSim ps m c step err = multiphase ps f step err
  where
    f = Frame 0 m dm c dc ::Frame
    (dm,dc) = (growthFunc intitgrowth) ienv m c 0 ::(Double,Double)
    (Dif (ienv,intitgrowth,_)) = ps!!0

multiphase :: [Phase] ->  Frame -> Double -> Double -> [Simulation]
multiphase [] _ _ _ = []
multiphase ((Dif p):ps) f step err = thisSim:(multiphase ps ef step err)
  where
    (env,gf,con) = p
    thisSim =  Simulation (condFramegen con step err env gf f) env
    ef = endState thisSim
multiphase (Dynamic p:ps) f step err = DyStep ef :multiphase ps ef step err
  where
    ef = p f

condFramegen :: (Frame -> Bool) -> Double -> Double -> Enviornment -> GrowthFunc -> Frame -> [Frame]
condFramegen cond step err env growthF f
  | not (cond f) && (adjStep > 0) = f : condFramegen cond step err env growthF nextFrame
  | otherwise = [f]
      where
        nextFrame = (smartStep growthF f env adjStep err) :: Frame
        adjStep =  (min step (days_ - t_))
        days_ = (days . season) env
        t_ = t f

endState::Simulation->Frame
endState (Simulation f _ ) = last f
endState (DyStep s)        = s

keyFrames::[Simulation]->[Frame]
keyFrames sims = concat [getFrames sim | sim <- sims]
  where
    getFrames::Simulation->[Frame]
    getFrames (DyStep sim) = [(stepFrame (DyStep sim))]
    getFrames sim          = frames sim


--phases
preSurface::Phase
preSurface = Dif (env,gf,preend)
  where
    env = Enviornment norwood genStrain (buildseason 50 25 80 50 115)
    gf = ubgrowth
    preend::Frame -> Bool
    preend f = (m f) > ((d (lake env)) * (ps (strain env)))

postSurface::Phase
postSurface = Dif (env,gf,postend)
  where
    env = Enviornment norwood genStrain (buildseason 100 25 80 50 115)
    gf = cwgrowth
    postend::Frame -> Bool
    postend f = (t f) >= 100

winter::Phase
winter = Dynamic (cold)
  where
    cold::Frame -> Frame -- also sets the season time back to 0
    cold f = (Frame 0 1 0 (0.6*c_) 0)
      where
        c_ = c f

seasonCarb:: [Phase] -> Double->Double->Double
seasonCarb ps x err =  (c . endState . last ) (wholeSim ps 1 x 1 err)

smartSeasonCarb:: [Phase] -> Double->Double->Double
smartSeasonCarb ps x err = numLim  err results
  where
    results = [(seasonCarb ps x (err/(2^(n)))) | n <- [1..] ]


genFuture:: [Phase] -> Double->Double->[Double]
genFuture ps x err = x : (genFuture ps (smartSeasonCarb ps err x) err)

stable:: [Phase] -> Double -> Double -> Double
stable ps x err = numLim err (genFuture ps x err)

numLim::Double -> [Double] -> Double
numLim eps ls = snd $ head $ filter difcheck ps --gabe wrote this function for me
  where
    ps = (zip ls (tail ls))
    difcheck = (\(x,y) -> (abs (x-y)) < eps)

simdata::[Phase] -> Double -> Double -> Double -> Double -> [[Double]]
simdata  ps err start step stop = [ [x,(smartSeasonCarb ps x err),((smartSeasonCarb ps x err)-(smartSeasonCarb ps (x+eps) err))/eps] | x<- [start,(start+step)..stop] ]
  where
    eps=0.000000001

standPhase::[Phase]
standPhase = [preSurface,postSurface,winter]

wevilReduct::Double->Double->Double
wevilReduct x err = stable (mapN 2 (wevilPhase x) standPhase ) 0.1 err

dynamicWevilReduct::Double->Double->Double
dynamicWevilReduct = (dynamify wevilReduct (\x y -> abs (x-y) ) )

mapN::Int->(a->a) -> [a] -> [a]
mapN n f xs = (map f (take n xs)) ++ (drop n xs)

wevilPhase::Double->Phase->Phase
wevilPhase x (Dif (e,g,p)) = (Dif (e , wevil x g,p))

main::IO ()
main = funcToWrite (\a b c d -> map (flip dynamicWevilReduct d) [a,a+b..c])
