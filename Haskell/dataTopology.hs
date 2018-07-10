import SimLibs.MainCarbsimlib
import System.Environment
import System.IO
import Topology.Complex

--- generate data
makeGrowthSurface:: Double -> Double -> [[Double]]
makeGrowthSurface m t = fmap ptgrowth (make2Sameple [0..m]  [0..t])

ptgrowth:: [Double] -> [Double]
ptgrowth (m:t:[]) = [m,dm,tmp]
  where
    (dm,dc) = ((growthFunc newgrowth) norwoodEnv) m 0 t
    tmp = ((temp . season) norwoodEnv) t

make2Sameple:: [Double] -> [Double] -> [[Double]]
make2Sameple as bs = [ [a,b] | a<-as , b<-bs]





main = do
    instr <- getArgs
    {- Can be used to create the file directly
    let contents =  show( function  (read (instr!!1) :: Double) (read (instr!!2) :: Double) (read (instr!!3) :: Double) (read (instr!!3) :: Double) )
    writeFile (instr!!0) contents
    -}
    putStrLn ( show( 1)) --wholeFunc (read (instr!!0) :: Double) (read (instr!!1) :: Double) (read (instr!!2) :: Double)  ))
