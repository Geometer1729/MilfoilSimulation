module Haskell.Util.Norm (numLim,dynamify) where

class Normed a where
  norm :: a -> a -> Double

instance Normed Double where
  norm = (abs .) . (-)

instance Normed a => Normed [a] where
  norm a b = sqrt $ sum $  map (^2) $ zipWith norm a b

numLim:: (Normed a) => Double -> [a] -> a
numLim eps xs = head final
  where
    pairs = zipWith (\x y -> (x,y,norm x y) ) xs (tail xs)
    final = [y | (x,y,n) <- pairs , n < eps]

dynamify:: Normed b =>  (a -> Double ->  b) -> a -> Double ->  b
dynamify func input tol = rollingDynamify func tol tol input

rollingDynamify:: Normed b => (a -> Double ->  b) -> Double -> a ->  Double -> b
rollingDynamify func tol try input = if dist < tol then next else rollingDynamify func tol (try/2) input
  where
    this = func input try
    next = func input (try/2)
    dist = norm this next
