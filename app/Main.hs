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
    let (playerHand, d') = dealCard emptyHand d
    let (dealerHand, d) = dealCard emptyHand d'
    putStr "Dealer shows: "
    print dealerHand
    let (playerHand', d') = dealCard playerHand d
    let (dealerHand', d) = dealCard dealerHand d'
    (playerHand, d') <- playerTurn playerHand' d
    if handType playerHand /= Bust
        then do
            (dealerHand, d) <- dealerTurn dealerHand' d'
            if handType dealerHand /= Bust
                then do
                    case compare (total playerHand) (total dealerHand) of
                        LT -> print "You lose!"
                        GT -> print "You win!"
                        EQ -> print "Push!"
                    return ()
                else print "You win!"
        else print "You lose!"
    return ()
