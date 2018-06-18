module Topology.BettyNum where

import Topology.Complex
import Topology.Tries

data Sparse = Sparse (Int,Int) [(Int,Int,Int)] deriving(Show)

boundryMap:: (Int,[STrie Int]) -> (Int,[STrie Int]) -> Sparse
boundryMap (d1,larger) (d2,smaller) = Sparse (s,l) ents
  where
    s = treeListSize smaller :: Int
    l = treeListSize larger :: Int
    simps = [ (ind,(simpBoundry (indexTree larger ind))) | ind <- [0..(l-1)] ] :: [(Int,[([Int],Int)])]
    ents = simpsToEnts smaller simps

simpsToEnts:: [STrie Int] -> [(Int,[([Int],Int)])] -> [(Int,Int,Int)]
simpsToEnts smaller ([]) = []
simpsToEnts smaller ((lg,[]):xs) = simpsToEnts smaller xs
simpsToEnts smaller ((lg,(simp,val):simps):xs) = (lg,sm,val) : ( simpsToEnts smaller ((lg,simps):xs) )
  where
    sm = (getIndex smaller simp)


treeListSize:: [STrie a] -> Int
treeListSize ts = sum (fmap getSize ts)

simpBoundry:: [Int] -> [([Int],Int)]
simpBoundry xs = withSigns
  where
    subs = [remove x xs | x <- xs]
    withSigns = zip subs [(-1)^n | n <- [1..]]

dim::Complex -> Int
dim (Complex xs) = length xs
