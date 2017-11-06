module BlackJack where
import Cards
import RunGame
{-Task 3.2
size hand2
  = size (Add(Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty))
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2
-}
empty :: Hand
empty = Empty

-- | A ranks value according to Black Jack rules
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric x) = x
valueRank _ = 10

-- | A cards value according to Black Jack rules
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

-- | We want to check how many aces a player has in her hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace s) hand) = 1 + numberOfAces hand
numberOfAces (Add card hand) = 0 + numberOfAces hand

--value :: Hand -> Integer
--value Empty = 0
--value (Add card hand) = __ + value hand

--gameOver :: Hand -> Bool

--winner :: Hand -> Hand -> Player
