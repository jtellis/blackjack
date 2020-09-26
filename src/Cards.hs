module Cards (
    deck
  , shuffle
  , addCard
) where

import System.Random (RandomGen, randomR)

data Rank = Ace | Two | Three | Four | Five
          | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King
        deriving (Show, Enum, Eq)

data Suit = Clubs | Diamonds | Hearts | Spades
        deriving (Show, Enum, Eq)

data Card = Card { suit :: Suit
                 , rank :: Rank }
        deriving (Eq)

instance Show Card where
    show (Card s r) = show r ++ " of " ++ show s

type Deck = [Card]

-- Any hand with an Ace valued as 11 is called a ‘soft’ hand.
-- All other hands are ‘hard’ hands.
-- A hand with a total value greater than 21 is a 'bust'.
data HandType = Soft | Hard | Bust deriving (Show, Eq)

data Hand = Hand { cards :: [Card]
                 , handType :: HandType
                 , total :: Int
                 }
    deriving (Show)

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

addCard :: Hand -> Card -> Hand
addCard h c
    -- If hand is soft and Ace valued 1 doesn't result in bust, then hand is still soft
    | (isAce && isSoft) && total h + 1 <= 21 = hand cards' Soft $ total h + 1
    -- If hand is hard and Ace valued 11 doesn't result in bust, then hand is now soft
    | (isAce && isHard) && total h + 11 <= 21 = hand cards' Soft $ total h + 11
    -- If hand is hard and Ace valued 1 doesn't result in bust, then hand is still hard
    | (isAce && isHard) && total h + 1 <= 21 = hand cards' Hard $ total h + 1
    -- If hand is hard and Ace valued 1 does result in bust, then hand is now bust
    | (isAce && isHard) && total h + 1 > 21 = hand cards' Bust $ total h + 1
    -- If hand is soft and card's value doesn't result in bust, then hand is still soft
    | isSoft && total h + value c <= 21 = hand cards' Soft $ total h + value c
    -- If hand is soft and the Ace currently valued at 11 is instead valued at 1 and
    --   new card's value doesn't result in bust, then hand is now hard
    | isSoft && total h - 10 + value c <= 21 = hand cards' Hard $ total h - 10 + value c
    -- If hand is hard and card's value doesn't result in bust, then hand is still hard
    | total h + value c <= 21 = hand cards' Hard $ total h + value c
    -- If hand is hard and card's value does result in bust, then hand is now bust
    | otherwise = hand cards' Bust $ total h + value c
    where isAce = rank c == Ace
          isSoft = handType h == Soft
          isHard = handType h == Hard
          cards' = c:cards h
          hand = \cs ht t -> Hand {cards=cs, handType=ht, total=t}
          value Card {rank=Two} = 2
          value Card {rank=Three} = 3
          value Card {rank=Four} = 4
          value Card {rank=Five} = 5
          value Card {rank=Six} = 6
          value Card {rank=Seven} = 7
          value Card {rank=Eight} = 8
          value Card {rank=Nine} = 9
          value _ = 10
