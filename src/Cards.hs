module Cards (
    deck
  , pick
) where

import System.Random

data Rank = Ace | Two | Three | Four | Five
          | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King
        deriving (Show, Enum)

data Suit = Clubs | Diamonds | Hearts | Spades
        deriving (Show, Enum)

data Card = Card { suit :: Suit
                 , rank :: Rank }

instance Show Card where
    show (Card s r) = show r ++ " of " ++ show s

type Deck = [Card]

deck :: Int -> Deck
deck n = concat $ replicate n deck
    where deck = Card <$> [Clubs .. Spades] <*> [Ace .. King]

pick :: (RandomGen g) => Deck -> g -> (Card, g)
pick deck gen =
    let n = length deck
        (i, newGen) = randomR (0, n - 1) gen
    in (deck !! i, newGen)