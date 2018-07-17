module Haskell.Util.Norm where

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
