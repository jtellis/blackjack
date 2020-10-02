module Gameplay (
    playerTurn,
    dealerTurn
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
