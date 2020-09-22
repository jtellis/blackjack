module Main where

import Cards
import System.Random  

main :: IO ()
main = do
    gen <- getStdGen
    print $ pick (deck 1) gen