import Data.List
import System.IO
import Debug.Trace

tracethis::(Show a) => a->a
tracethis x = trace (show x) x


minx::Double
minx = 44.7225
maxx::Double
maxx = 44.7312
miny::Double
miny = (-75.0164)
maxy::Double
maxy = (-74.9892)
step::Double
step = 0.001




main::IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let dat = doStuff contents
  putStrLn (show dat)
  putStrLn (show (betterInfrencce dat [44.726,74.99]) )
  let dmap = map (\x -> x++[betterInfrencce dat x]) (grid [minx,minx+step..maxx] [miny,miny+step..maxy] )
  writeFile  "../../Python/depth.txt"  (show dmap)
  hClose handle



csv::[[Double]]->[Char]
csv = joinlines . (fmap (comalines))

floorshowbylines::[Double] -> [Char]
floorshowbylines (x:xs) = ( ((split '.'  (show x))!!0) ++ "\n") ++ (floorshowbylines xs)
floorshowbylines [] = []

showbylines::(Show a)=>[a] -> [Char]
showbylines (x:xs) = ( (show x) ++ "\n") ++ (showbylines xs)
showbylines [] = []

joinlines::[[Char]] -> [Char]
joinlines (x:xs) = ( ((reverse . tail . reverse) x) ++ "\n") ++ (joinlines xs)
joinlines [] = []

comalines::(Show a)=>[a] -> [Char]
comalines (x:xs) = preComaDrop
  where
    preComaDrop = ( (show x) ++ ",") ++ (comalines xs)
comalines [] = []

evenMoreStuff::[Char]->[Double]
evenMoreStuff infile = toft
  where
    dat = doStuff infile
    rawDist = distances dat
    toft = fmap ((*365220)) rawDist

doStuff::[Char]->[[Double]]
doStuff infile = outfile
  where
    lines_ = split '\n' infile
    sliced = slice 3 lines_
    outfile =  fmap (fmap (read)) ((transpose . (fmap darn) . transpose) sliced )

split:: (Eq a) => a -> [a] -> [[a]]
split s xs = splithelper s [] xs
  where
    splithelper:: (Eq a) => a -> [a] -> [a] -> [[a]]
    splithelper s temp (x:xs)
      | (s == x) = temp : (splithelper s [] xs)
      | otherwise = splithelper s (temp ++ [x] ) xs
    splithelper _ temp [] = [temp]

darn::[[Char]]->[[Char]]
darn (l:ls) = l:(rollingDarn l ls)

rollingDarn::[Char]->[[Char]]->[[Char]]
rollingDarn base (l:ls) = newBase: (rollingDarn newBase ls)
  where
    newBase = rep base l
rollingDarn _  [] = []


rep::[Char]->[Char]->[Char]
rep base rep = (take (lb -lr) base) ++ rep
  where
    lb = length base
    lr = length rep

slice::Int -> [a] -> [[a]]
slice _ [] = []
slice n xs = (take n xs):(slice n (drop n xs))

dist::([Double] , [Double])->Double
dist (x,y) = sqrt ( ((x!!0)-(y!!0))^2+ ((x!!1)-(y!!1))^2)

distances::[[Double]]->[Double]
distances dat = fmap dist (pairOff dat)

pairOff::[a]->[(a,a)]
pairOff (x:y:xs) = (x,y):(pairOff (y:xs))
pairOff (x:[]) = []

cross::[Double]->[Double]->[Double]
cross (x1:x2:x3:[]) (y1:y2:y3:[]) = [x2*y3-x3*y2,x3*y1-x1*y3,x1*y2-x2*y2]

dot::[Double]->[Double]->Double
dot (x:xs) (y:ys) = x*y+ (dot xs ys)
dot [] [] = 0

triInfrence:: [[Double]] -> [Double] ->  Double
triInfrence dat v = z
  where
    (p1:p2:p3:[]) = take 3 byDist :: [[Double]]
    byDist = sortBy (\v1 v2 -> (compare (dist (v1,v)) (dist (v2,v)) )) dat ::[[Double]]
    v1 = zipWith (-) p1 p2 ::[Double]
    v2 = zipWith (-) p1 p3 ::[Double]
    n = cross v1 v2:: [Double]
    d = dot n p1 :: Double
    z = -(d - ((n!!0)*(v!!0)+(n!!1)*(v!!1)))/(n!!2) :: Double

betterInfrencce::[[Double]] -> [Double] ->  Double
betterInfrencce dat pt = result
  where
    distVal = map (\x -> (dist (x,pt) , (x!!2))) dat
    sorted = sortBy (\(x1,y1) (x2,y2) -> compare x1 x2) distVal
    afew = take 5 sorted
    weight =  map (\(x,y) -> ( exp((-1)*(x^2)) ,y) ) afew
    totalWeight = tracethis (sum (map fst weight))
    result = sum (map (\(x,y) -> y*(x/totalWeight)) weight)

grid::[Double]->[Double]->[[Double]]
grid xs ys = [ [x,y] | x<- xs , y<-ys]
