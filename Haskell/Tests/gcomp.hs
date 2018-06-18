import MainCarbsimlib
import System.Environment


function :: Double -> Double -> Double -> Double -> [[Double]]
function step err m_ c_ = zipWith (zipWith (-)) (siminterp norwoodEnv oldgrowth step err m_ c_) (siminterp norwoodEnv newgrowth step err m_ c_)

main = do
    instr <- getArgs
    putStrLn (show(function (read (instr!!0) :: Double) (read (instr!!1) :: Double) (read (instr!!2) :: Double) (read (instr!!3) :: Double) ))
