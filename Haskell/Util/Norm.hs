module Haskell.Util.Norm where

class Normed a where
  norm :: a -> a -> Double

instance Normed Double where
  norm = (abs .) . (-)

instance Normed a => Normed [a] where
  norm a b = sqrt $ sum $  map (^2) $ zipWith norm a b
