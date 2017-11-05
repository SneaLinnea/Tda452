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


--value :: Hand -> Integer
--value Empty = 0
--value (Add card hand) = __ + valueCard hand

--gameOver :: Hand -> Bool

--winner :: Hand -> Hand -> Player
