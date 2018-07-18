{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskell.Util.FuncToIO (funcToExe , funcToWrite ,funcToPlot) where

import           System.Environment
import           System.Process

data MaybeFuncTo a = FuncTo (String -> MaybeFuncTo a) | Plain a

funcToExe :: (Cast a) => a-> IO ()
funcToExe f = do
    args <- getArgs
    putStrLn ( app (cast f) args)

funcToWrite :: (Cast a) => a -> IO ()
funcToWrite f = do
    args <- getArgs
    let fileName = head args
    writeFile fileName (app (cast f) (drop 1 args))

funcToPlot :: ( Plotable a) => a -> IO ()
funcToPlot f = do
      args <- getArgs
      app (castPlot f) args

app :: MaybeFuncTo a -> [String] -> a
app (FuncTo f) (x:xs) = app (f x) xs
app (Plain x) []      = x
app _ []              = error "Not enough arguments"
app (Plain _ ) _      = error "Too many arguments"

class Cast a where
    cast :: a -> MaybeFuncTo String

instance (Read a, Cast b) => Cast ( a -> b ) where
    cast f = FuncTo (cast . f . read)

instance Show a => Cast a where
    cast x = Plain (show x)

class Plotable a where
  castPlot:: a -> MaybeFuncTo (IO ())

instance (Read a, Plotable b) => Plotable ( a -> b ) where
    castPlot f = FuncTo (castPlot . f . read)

instance Plotable [Double] where
  castPlot dat = Plain (do
    writeFile "Data.txt" (show dat)
    putStrLn "On to python"
    writeFile "plotfile.py" plotListCode
    callCommand "python plotfile.py" )

plotListCode::String
plotListCode = concat [ x ++ "\n" | x<- xs]
  where
    xs = ["import matplotlib.pyplot as plt" , "import numpy as np", "from mpl_toolkits.mplot3d import Axes3D",  teribleString ,  "out= (''.join ( [c for line in file for c in line] [1:-1] ))" ,  "x = [float(x) for x in out.split(',')]",  "file.close()",  "plt.plot(range(0,len(x)),x)", "plt.show()" ] ::[String]
    teribleString = "file = open(\"data.txt\",)" :: String

instance Plotable [(Double,Double)] where
  castPlot dat = Plain ( do
    writeFile "Data.txt" (show dat)
    putStrLn "On to python"
    writeFile "plotfile.py" plotListTupCode
    callCommand "python plotfile.py" )

plotListTupCode::String
plotListTupCode = concat [ x ++ "\n" | x<- xs]
  where
    xs = ["import matplotlib.pyplot as plt" , "import numpy as np", "from mpl_toolkits.mplot3d import Axes3D" , teribleString , "out= (''.join ( [c for line in file for c in line] [2:-2] ))" , "x = np.transpose ( [ [ float(p) for p in tup.split(',') ] for tup in out.split('),(')] )" , "file.close()" , "plt.scatter(x[0],x[1])" , "plt.show()" ]
    teribleString = "file = open(\"data.txt\",)"

instance Plotable [(Double,Double,Double)] where
  castPlot dat = Plain ( do
    writeFile "Data.txt" (show dat)
    putStrLn "On to python"
    writeFile "plotfile.py" plot3DCode
    callCommand "python plotfile.py" )

plot3DCode::String
plot3DCode = concat [ x ++ "\n" | x<- xs]
  where
    xs =[ "import matplotlib.pyplot as plt","import numpy as np","from mpl_toolkits.mplot3d import Axes3D", teribleString ,"out= (''.join ( [c for line in file for c in line] [2:-2] ))" , "x = np.transpose ( [ [ float(p) for p in tup.split(',') ] for tup in out.split('),(')] )" ,  "file.close()" , "ax = plt.figure().add_subplot(111, projection='3d')" , "ax.scatter(x[0],x[1],x[2])" ,  "plt.show()" ]
    teribleString = "file = open(\"data.txt\",)"
