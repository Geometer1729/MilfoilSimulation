module Topology.Tries where

data Trie a = Leaf a | Node a [(Trie a)]
data STrie a = SLeaf a | SNode (Int,a) [STrie a]  deriving(Show) --Tree that knows it's size usefull for indexing


instance (Show a) => Show (Trie a) where
  show t = shw 0 t

shw:: (Show a) => Int -> Trie a -> [Char]
shw depth (Leaf l) = concat ["\n",(repList "    " depth),"L ",show(l)]
shw depth (Node n ts) = concat (["\n",(repList "    " depth),"N ",show(n)]  ++ [shw (depth+1) t | t<-ts])

repList::[a] -> Int -> [a]
repList xs 0 = []
repList xs n = xs ++ (repList xs (n-1))

treeFromList:: (Eq a) => [[a]] -> [Trie a]
treeFromList (x:xs) = treeBuild start xs
  where
    start = startTree x
treeFromList [] = []

treeBuild:: (Eq a) => [Trie a] -> [[a]] -> [Trie a]
treeBuild tree (l:[]) = treeAdd tree l
treeBuild tree (l:ls) = treeBuild (treeAdd tree l) ls
treeBuild tree [] = tree

treeAdd:: (Eq a) => [Trie a] -> [a] -> [Trie a]
treeAdd ((Node n ns):[]) (x:xs)
  | (n == x) = ((Node n (treeAdd ns xs) ):[])
  |otherwise = (Node n ns):(startTree (x:xs))
treeAdd ((Node n ns):ts) (x:xs)
  | (n == x) = ((Node n (treeAdd ns xs) ):ts)
  | otherwise = (Node n ns) : (treeAdd ts (x:xs))
treeAdd (Leaf l:ls) (x:[])
  | (l == x) = (Leaf l):(Leaf x):ls
  | otherwise = (Leaf l):(treeAdd ls (x:[]))
treeAdd [] xs = startTree xs

startTree:: [a] -> [Trie a]
startTree (x:[]) = [Leaf x]
startTree (x:xs) = [ Node x (startTree xs) ]

treeSizer:: Trie a -> STrie a
treeSizer (Node n ns) = (SNode (size,n) subTrees)
  where
    subTrees = fmap treeSizer ns
    size = sum $ fmap getSize subTrees
treeSizer (Leaf x) =  SLeaf x

getSize:: STrie a -> Int
getSize (SLeaf x) = 1
getSize (SNode (s,x) xs) = s

upTree:: (Int,Int) -> Trie Int
upTree (0,n) = Leaf n
upTree (d,n) = Node n [upTree ((d-1),x) | x <- [0..n]]

getLeaf::Trie a -> a
getLeaf (Leaf x) = x

has:: [Trie Int] -> [Int] -> Bool
has ((Node n ns):ts) (x:xs)
  |(n == x) = has  ns xs
  |otherwise = has ts (x:xs)
has (Leaf l:ls) (x:[])
  | l == x = True
  | otherwise = has ls (x:[])
has l [] = False
has _ _ = error "checked list of the wrong length"

treeLength:: [Trie Int] -> Int
treeLength ((Node a ns):ts) = treeLength ns + treeLength ts
treeLength [] = 0
treeLength ((Leaf a):ls) = 1 + treeLength ls

getIndex:: (Eq a) => [STrie a] -> [a] ->  Int
getIndex ((SNode (s,n) ns):ts) (x:xs)
  | x==n = getIndex ns xs
  | otherwise = s + ( getIndex  ts (x:xs) )
getIndex  ((SLeaf l):ls) (x:[])
  | x==l = 0
  | otherwise = 1 + (getIndex  ls (x:[]) )
getIndex _ _ = error "Couldn't find list"

indexTree:: [STrie a] -> Int -> [a]
indexTree ((SNode (i,n) ns):ts) ind
  | i <= ind = indexTree ts (ind - i)
  | i > ind = n:(indexTree ns ind)
indexTree ((SLeaf l):ls) ind
  | ind == 0 = [l]
  | otherwise = indexTree ls (ind-1)
