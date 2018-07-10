import           Haskell.Util.FuncToIO
import           Haskell.MainCarbsimlib
import           System.Environment

function :: Double -> Double -> Double -> Double -> [[Double]]
function step err m_ c_ = zipWith (zipWith (-)) (siminterp norwoodEnv ubgrowth step err m_ c_) (siminterp norwoodEnv cwgrowth step err m_ c_)

main = funcToExe function
