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

value :: Hand -> Integer

gameOver :: Hand -> Bool

winner :: Hand -> Hand -> Player
