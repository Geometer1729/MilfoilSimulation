import Mainsimlib
import System.Environment


pt:: Enviornment -> Double -> Double -> (Double,Double,Double)
pt env m t = ((growth env m t), m , (temp (season env) t) )


ptByT:: Enviornment  -> [Double] -> Double -> [(Double,Double,Double)]
ptByT env ts m = fmap (pt env m) ts

ptGen:: Enviornment -> [Double] -> [Double] -> [(Double,Double,Double)]
ptGen env ms ts = concat (fmap (ptByT env ts) ms)

f:: Double -> Double -> Double -> Double -> Double -> Double -> [(Double,Double,Double)]
f msr msp men tsr tsp ten = ptGen norwoodEnv [msr,msr+msp .. men] [tsr,tsr+tsp .. ten]

main = do
    instr <- getArgs
    putStrLn (show(f (read (instr!!0) :: Double) (read (instr!!1) :: Double) (read (instr!!2) :: Double) (read (instr!!3) :: Double) (read (instr!!4) :: Double) (read (instr!!5) :: Double) ))
