import FuncToIO

test:: Double -> Int -> [Int] -> (Double,Int,[Int])
test a b c = (sqrt a, b ^ 2 , take 12 (cycle c))

main = funcToExe test
