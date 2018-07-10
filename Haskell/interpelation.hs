module Interpelation where

--ts are new domain
--xs are original domain


findFrames :: [Double] -> [Double] -> [a] -> [a]
findFrames (t:ts) (x1:x2:xs) (a:as)
  |thisgap < nextgap = a : findFrames ts (x1:x2:xs) (a:as)
  |nextgap <= thisgap = findFrames (t:ts) (x2:xs) as
  where
    thisgap = abs (x1-t)
    nextgap = abs (x2-t)
findFrames [] _ _ = []
findFrames (t:ts) (x:[]) (a:[]) = a : findFrames ts (x:[]) (a:[])

interpelate:: [Double] -> [a] -> [(a->Double)] -> [(a->Double)] -> (a->Double) -> [[Double]]
interpelate ts allFrames valFuncs dirFuncs timeFunc = interp
  where
    frames = findFrames ts xs allFrames
    vals   = [ [valfunc frame | valfunc <- valFuncs] | frame <- frames]
    derivs = [ [dirfunc frame | dirfunc <- dirFuncs] | frame <- frames]
    jumps  = zipWith (-) ts (fmap timeFunc frames)
    interp = zipWith (zipWith (+)) vals [[deriv * jump | deriv <- derivframe] | (jump,derivframe) <- (zip jumps derivs)]
    xs = fmap timeFunc allFrames
