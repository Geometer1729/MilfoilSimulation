{-# LANGUAGE FlexibleInstances , UndecidableInstances , IncoherentInstances , MonoLocalBinds #-}

module FuncToIO.FuncToIO (funcToExe , funcToWrite) where

import System.IO
import System.Process
import System.Environment

data MaybeFuncTo a = FuncTo ([Char]->(MaybeFuncTo a)) | Plain a

app :: MaybeFuncTo [Char] -> [[Char]] -> [Char]
app (FuncTo f) (x:xs) = app (f x) xs
app (Plain x) [] = x
app _ [] = error "Not enough arguments"
app (Plain x) _ = error "Too many arguments"

class Cast a where
    cast :: a -> MaybeFuncTo [Char]

instance (Read a, Cast b) => Cast ( a -> b ) where
    cast f = FuncTo (\x -> (cast (f (read x))))

instance Show a => Cast a where
    cast x = Plain (show x)

funcToExe :: (Cast a) => a-> IO ()
funcToExe f = do
    args <- getArgs
    putStrLn ( (app (cast f)) args)

funcToWrite :: (Cast a) => a -> IO ()
funcToWrite f = do
    args <- getArgs
    let fileName = args!!0
    writeFile fileName ((app (cast f)) (drop 1 args))
