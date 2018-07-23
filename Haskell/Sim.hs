import           Haskell.Phases
import           Haskell.Util.FuncToIO
import           Haskell.Util.Mixed
import           Haskell.Util.RK4
import           Haskell.Util.SimpleWevil

sim:: Int -> Double -> Double -> Double -> [Frame]
sim years tol wm wc = multiPhase tol 1 (take (3*years) ( cycle (wevilModel wm wc))) (0,[(1,0),(0,0)])

getPlotable:: [Frame] -> [(Double,Double)]
getPlotable = fmap (\ (t,[(m,_),(c,_)]) -> (m,c) )



--main = funcToPlot (\a b c d -> getPlotable (sim a b c d))
main = funcToPlot createMap
