module Main where

import Cards
import System.Random  

main :: IO ()
main = do
    gen <- getStdGen
    let (d, gen') = shuffle (deck 1) gen
    let (hand, d') = deal emptyHand d
    let (hand', d'') = deal hand d'
    putStrLn $ show hand ++ " => " ++ show hand'
