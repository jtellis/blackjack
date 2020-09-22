module Main where

import Cards
import System.Random  

main :: IO ()
main = do
    gen <- getStdGen
    print $ shuffle (deck 1) gen