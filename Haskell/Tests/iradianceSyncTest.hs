import Mainsimlib
import System.Environment

altSeason:: Double -> Season
altSeason phi = buildseason 200 25 phi 50 phi

altEnv:: Double -> Enviornment
altEnv phi = Enviornment norwood genStrain (altSeason phi)

err:: Enviornment -> Double -> Double -> Double -> Double -> Double
err  env step err phi m = maximum (zipWith posdif (siminterp env step err m) (siminterp (altEnv phi) step err m))
  where posdif :: Double -> Double -> Double
        posdif x  y = abs (x-y) :: Double

errOfPhi:: Double -> Double
errOfPhi phi = maximum (fmap (err norwoodEnv 1 0.0000000001 phi) [0,1..100])

datOfRes:: Double -> Double -> Double -> [Double]
datOfRes center range res = fmap errOfPhi [center-range,center-range+res .. center+range]

main = do
    instr <- getArgs
    putStrLn (show(datOfRes (read (instr!!0) :: Double) (read (instr!!1) :: Double) (read (instr!!2) :: Double) ))
