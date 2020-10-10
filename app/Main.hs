module Main where

import qualified Terminal as Term
import Cards
import Gameplay
import System.Random

main :: IO ()
main = do
    Term.clear
    gen <- getStdGen
    let (d, gen') = shuffle (deck 1) gen
    playLoop d []
    return ()
