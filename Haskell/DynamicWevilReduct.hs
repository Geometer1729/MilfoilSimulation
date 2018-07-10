import           Haskell.PhaseLib
import           Haskell.Util.FuncToIO

main::IO ()
main = funcToPlot ( \a b c d -> map (`dynamicWevilReduct` d) [a,a+b..c] )
