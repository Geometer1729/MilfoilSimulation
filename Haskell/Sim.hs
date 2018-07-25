import           Haskell.Phases
import           Haskell.Util.FuncToIO
import           Haskell.Util.Mixed
import           Haskell.Util.RK4
import           Haskell.Util.SimpleWevil

sim:: Int -> Double -> Double -> Double -> Double -> [Frame]
sim years seed tol wm wc = multiPhase tol 1 (take (3*years) ( cycle (wevilModel wm wc))) (0,[(seed,0),(0,0)])

getPlotable:: [Frame] -> [(Double,Double,Double)]
getPlotable = fmap (\ (t,[(m,_),(c,_)]) -> (t,c,m) )



--main = funcToPlot (\a b c d e -> getPlotable (sim a b c d e))
main = funcToPlot createMap
