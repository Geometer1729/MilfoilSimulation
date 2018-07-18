module Haskell.Util.Norm (Normed,norm,numLim,dynamify) where

class Normed a where
  norm :: a -> a -> Double

instance Normed Double where
  norm a b = if isNaN a && isNaN b then 0 else abs (a-b)

instance (Normed a, Show a) => Normed [a] where
  norm a b = sqrt $ sum $ map (^2) $ zipWith norm a b

numLim:: (Normed a) => Double -> [a] -> a
numLim eps xs = head final
  where
    pairs = zipWith (\x y -> (x,y,norm x y) ) xs (tail xs)
    final = [y | (x,y,n) <- pairs , n < eps]

dynamify:: Normed b =>  (a -> Double ->  b) -> a -> Double ->  b
dynamify func input tol = rollingDynamify func tol input tol

rollingDynamify:: Normed b => (a -> Double ->  b) -> Double -> a ->  Double -> b
rollingDynamify func tol input try = if dist < tol then next else rollingDynamify func tol input (try/2)
  where
    this = func input try
    next = func input (try/2)
    dist = norm this next
