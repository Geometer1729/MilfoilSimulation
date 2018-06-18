module Topology.Complex where

import Control.Monad
import Data.List
import Topology.Tries

{-
Much of this code is prone to catestrophic faliure if anything ever isn't sorted!
If you are expanding this and need to do somthing that scrambles a list
you should sort everything back afterward
-}

data Complex = Complex [ (Int,[Trie Int]) ] deriving(Show)

dataToComplex:: [[Double]] -> Double -> Complex
dataToComplex pts epsilon = Complex $ (1,base1):(2,base2): (nextLayers pts epsilon (2,base2) )
  where
    base1 = fmap (\x -> Leaf x) ind :: [Trie Int]
    base2 = treeFromList [ [x,y] | x<-ind , y <- ind , x<y && checkPoint pts epsilon [x,y] ]
    ind = [0..((length pts)-1)]

nextLayers:: [[Double]] -> Double -> (Int,[Trie Int]) -> [ (Int,[Trie Int]) ]
nextLayers pts epsilon layer
  | ((length newLay) == 0) = []
  | otherwise = nextLayer_ : (nextLayers pts epsilon nextLayer_)
  where
    (depth,newLay) = nextLayer_
    nextLayer_ = nextLayer pts epsilon layer

nextLayer:: [[Double]] -> Double -> (Int,[Trie Int]) ->(Int,[Trie Int])
nextLayer pts epsilon (depth,start) = (depth+1,newLayer)
  where
    candidatePairs = concat $ fmap listPairs start :: [([Int],[Int])]
    candidates = fmap (\(x,y) -> (union x y)) candidatePairs :: [[Int]]
    validCandidates = [cand | cand <- candidates, validate cand start]
    layerAsLists = [x | x<- validCandidates , (checkPoint pts epsilon x)] :: [[Int]]
    newLayer = treeFromList layerAsLists

listPairs:: (Ord a) => Trie a -> [([a],[a])]
listPairs (Node n (Leaf a:as)) = fmap (appendBoth) pairs
  where
    leavs = (Leaf a):as
    pairs = [([getLeaf x],[getLeaf y]) | x<-leavs , y<-leavs , ((getLeaf x) < (getLeaf y)) ]
    appendBoth = (\(x,y) -> (n:x,n:y))
listPairs (Node n ts) =  concat $ fmap ((fmap appendBoth) . listPairs) ts
  where
    appendBoth = (\(x,y) -> (n:x,n:y))

validate:: [Int] -> [Trie Int] -> Bool
validate simp graph = foldr (&&) True valids
  where
    subs = [remove s simp | s <- simp]
    valids = fmap (has graph) subs

remove  :: Eq a => a -> [a] -> [a]
remove blah xs = [ x | x <- xs, x /= blah]

checkPoint :: [[Double]] -> Double -> [Int] -> Bool
checkPoint  _ _ [] = True
checkPoint allpts epsilon ind  = furthest < epsilon
  where
    pts = fmap (allpts!!) ind
    furthest = maximum [l2 x y| x <- pts, y <- pts]

l2::[Double] -> [Double] -> Double
l2 xs ys = dist
  where
    dif = zipWith (-) xs ys
    sqr = fmap (\x->x^2) dif
    dist = sqrt( sum sqr )
