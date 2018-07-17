import           Haskell.Phases
import           Haskell.Util.FuncToIO
import           Haskell.Util.Mixed
import           Haskell.Util.RK4

sim::Double -> Double -> Double -> Double -> [Frame]
sim tol step m c = multiPhase tol step [preSurface,postSurface,winter] (0,[(m,0),(c,0)])

getPlotable:: [Frame] -> [Double]
getPlotable = fmap (fst . head . snd)


main = funcToPlot (\a b c d -> getPlotable (sim a b c d))
