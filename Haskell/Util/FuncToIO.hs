{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskell.Util.FuncToIO (funcToExe , funcToWrite, funcToPlot) where

import           System.Environment
import           System.Process

data MaybeFuncTo a = FuncTo (String -> MaybeFuncTo a) | Plain a

app :: MaybeFuncTo String -> [String] -> String
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

funcToExe :: (Cast a) => a-> IO ()
funcToExe f = do
    args <- getArgs
    putStrLn ( app (cast f) args)

funcToWrite :: (Cast a) => a -> IO ()
funcToWrite f = do
    args <- getArgs
    let fileName = head args
    writeFile fileName (app (cast f) (drop 1 args))

funcToPlot :: (Cast a) => a -> IO ()
funcToPlot f = do
  args <- getArgs
  writeFile "Data.txt" (app (cast f) args)
  putStrLn "On to python"
  writeFile "plotfile.py" plotFileCode
  callCommand "python plotfile.py"

plotFileCode::String
plotFileCode = concat [ x ++ "\n" | x<- xs]
  where
    xs = ["import matplotlib.pyplot as plt" , "import numpy as np",    "from mpl_toolkits.mplot3d import Axes3D",  teribleString ,  "out= (''.join ( [c for line in file for c in line] [1:-1] ))" ,  "x = [float(x) for x in out.split(',')]",  "file.close()",  "plt.plot(range(0,len(x)),x)", "plt.show()" ] ::[String]
    teribleString = "file = open(\"data.txt\",)" :: String
