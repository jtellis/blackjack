module Gameplay (
    playLoop
) where

import System.IO (hFlush, stdout)
import qualified Terminal as Term
import Cards

data PlayerAction = Hit | Stay deriving (Show, Eq)

encodeAction :: String -> Maybe PlayerAction
encodeAction s
    | s == "H" || s == "h" = Just Hit
    | s == "S" || s == "s" = Just Stay
    | otherwise = Nothing

getLnPrompt :: String -> IO String
getLnPrompt p = do
  putStr $ p ++ " -> " 
  hFlush stdout
  getLine

getAction :: IO PlayerAction
getAction = do
    s <- getLnPrompt "Hit or stay? [h/s]"
    maybe getAction return (encodeAction s)

playerTurn :: Hand -> Deck -> IO (Hand, Deck)
playerTurn h d
    | handType h == Bust = do
        putStr "Your hand: "
        print h
        return (h, d)
    | otherwise = do
        putStr "Your hand: "
        print h
        a <- getAction
        if a == Hit
            then do
                uncurry playerTurn $ dealCard h d
            else return (h, d)

dealerTurn :: Hand -> Deck -> IO (Hand, Deck)
dealerTurn h d
    | t < 17 || (isSoft && t == 17) = do
        putStr "Dealer's hand: "
        print h
        uncurry dealerTurn $ dealCard h d
    | otherwise = do
        putStr "Dealer's hand: "
        print h
        return (h, d)
    where isSoft = handType h == Soft
          t = total h

playRound :: Deck -> Discard -> IO (Deck, Discard)
playRound d discard = do
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
                else print "You win!"
            return (d, collectHands discard [playerHand, dealerHand])
        else do
            print "You lose!"
            return (d', collectHands discard [playerHand, dealerHand'])

playLoop ::  Deck -> Discard -> IO (Deck, Discard)
playLoop d discard = do
    putStrLn "--- NEW ROUND ---"
    (d', discard') <- playRound d discard
    playLoop d' discard'
