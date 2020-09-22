module Cards (
    deck
) where

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