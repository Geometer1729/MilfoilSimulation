import           Haskell.Util.FuncToIO
import           Haskell.MainCarbsimlib
import           System.Environment

startCtoEndM :: Double -> Double -> Double
startCtoEndM err c_ = m_ + (dm_ * (end - t_))
  where
    frame_ =  (last . frames) (simulate norwoodEnv 4 err 1 c_)
    end = (days . season) norwoodEnv
    t_ = t frame_
    m_ = m frame_
    dm_ = dm frame_


adaptiveSCEM :: Double -> Double -> Double
adaptiveSCEM err  = adaptiveSCEMs err err

adaptiveSCEMs ::Double -> Double -> Double -> Double
adaptiveSCEMs errmax errcur c_
  |err < errmax = result
  |otherwise = adaptiveSCEMs errmax (errcur/2) c_
  where
    result = startCtoEndM (errcur/2) c_
    err = result - startCtoEndM errcur c_

gendata :: Double -> Double ->[Double]
gendata err max_  = fmap (adaptiveSCEM err) [0,1..max_]




main = funcToExe gendata
