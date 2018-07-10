import Interpelation.Interpelation
import System.Environment
import System.IO
import FuncToExe.FuncToExe

testTree::Tree
testTree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

datToLFunc::[[Double]] -> [Double] -> [Double]
datToLFunc dat xs = pt_
  where
    pt = interpelate xs dat [(!!1)] [(!!2)] (!!0)
    pt_ = fmap (!!0) pt

datToFunc::[[Double]]->Double->Double
datToFunc dat x = pt_
  where
    pt = interpelate [x] dat [(!!1)] [(!!2)] (!!0)
    pt_ = (pt!!0)!!9

--sinks::(Double -> Double) -> [(Double,Double,Double)]




main:: IO ()
main=do
  args <- getArgs
  handle <- openFile (args!!0) ReadMode
  contents <- hGetContents handle
  putStrLn (show (datToLFunc (read contents :: [[Double]]) ([(read arg :: Double) | arg <- (drop 1 args) ])))
  hClose handle
