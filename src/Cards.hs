module Cards (
    deck
  , shuffle
) where

import System.Random (RandomGen, randomR)

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

shuffle :: (RandomGen g) => Deck -> g -> (Deck, g)
shuffle [card] gen = ([card], gen)
shuffle deck gen =
    let n = length deck
        (i, gen') = randomR (0, n - 1) gen
        (xs, ys) = splitAt i deck
        picked = head ys
        rest = xs ++ tail ys
        (shuffled, gen'') = shuffle rest gen'
    in (picked:shuffled, gen'')