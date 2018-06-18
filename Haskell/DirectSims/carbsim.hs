import MainCarbsimlib
import System.Environment


function :: Double -> Double -> [[Double]]
function = siminterp norwoodEnv newgrowth 1 0.00001


main = do
    instr <- getArgs
    putStrLn (show(function (read (instr!!0) :: Double) (read (instr!!1) :: Double) (read (instr!!2) :: Double) (read (instr!!3) :: Double) ))
