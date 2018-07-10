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
  callCommand "python plotfile.py"
