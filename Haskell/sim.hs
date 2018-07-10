import Mainsimlib
import System.Environment

f = siminterp norwoodEnv

main = do
    instr <- getArgs
    putStrLn (show(f (read (instr!!0) :: Double) (read (instr!!1) :: Double) (read (instr!!2) :: Double)))
