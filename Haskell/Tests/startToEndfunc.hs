import Mainsimlib
import System.Environment

dat:: Double -> Double -> Double -> Double -> [Double]
dat err min_ max_ step = fmap startToEnd [min_,min_+step .. max_]
  where
    startToEnd =  head . reverse . siminterp norwoodEnv 1 err

main = do
    instr <- getArgs
    putStrLn (show(dat (read (instr!!0) :: Double) (read (instr!!1) :: Double) (read (instr!!2) :: Double) (read (instr!!3) :: Double) ))
