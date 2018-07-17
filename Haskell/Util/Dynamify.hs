
module Haskell.Util.Dynamify (dynamify) where
import           Haksell.Util.Norm

dynamify:: Normed b =>  (a -> Double ->  b) -> a -> Double ->  b
dynamify func input tol = rollingDynamify func tol tol input

rollingDynamify:: Normed b => (a -> Double ->  b) -> Double -> a ->  Double -> b
rollingDynamify func tol try input = if dist < tol then next else rollingDynamify func tol (try/2) input
  where
    this = func input try
    next = func input (try/2)
    dist = norm this next
