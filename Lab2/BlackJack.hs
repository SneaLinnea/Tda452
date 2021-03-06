module BlackJack where
import Cards
import RunGame

import Test.QuickCheck hiding (shuffle)
import System.Random

{- Implementation -}
implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation

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

-- | Property: an empty hand is empty
prop_empty :: Bool
prop_empty = Empty == empty

-- | A ranks value according to Black Jack rules
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric x) |x < 2 = 2
                      |x > 10 = 10
                      |otherwise = x
valueRank _ = 10

-- | Property: value of Rank is >= 2 and <= 11
prop_valueTwoEleven :: Rank -> Bool
prop_valueTwoEleven r = (v <= 11) && (v >= 2)
  where v = valueRank r

-- | A cards value according to Black Jack rules
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

-- | We want to check how many aces a player has in her hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace s) hand) = 1 + numberOfAces hand
numberOfAces (Add card hand) = 0 + numberOfAces hand

-- | Property: number of aces <= than number of cards
prop_acesFewer :: Hand -> Bool
prop_acesFewer h = (numberOfAces h <= size h) && (numberOfAces h >= 0)

-- | The total value of a player's hand
-- | ace counts as 11
value' :: Hand -> Integer
value' Empty = 0
value' (Add card hand) = valueCard card + value hand

-- | Choose the highest value that is still < 21
-- | Ace is 1 or 11
value :: Hand -> Integer
value hand  |v > 21 = v - n*10
            |otherwise = v
          where
            n = numberOfAces hand
            v = value' hand

-- | Property: the value of a hand is less than the value with maxed Aces
prop_valueAndAces :: Hand -> Bool
prop_valueAndAces h | n == 0    = value h == value' h
                    | n == 1    = value h <= value' h
                    | otherwise = value h <  value' h
                where n = numberOfAces h

-- | The game is over if the hand's value is > 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- | Given one hand for the guest and one for the bank (in that order),
-- | which player has won?
winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest = Bank
                  | gameOver bank = Guest
                  | value guest <= value bank = Bank
                  | otherwise = Guest

-- | Property: Given equally valued hands the bank allways wins
prop_bankWinEqual :: Hand -> Hand -> Property
prop_bankWinEqual guest bank = value guest == value bank ==>
                                       winner guest bank == Bank

-- | Property: If the player busts the bank wins
prop_bankWinBust :: Hand -> Hand -> Property
prop_bankWinBust guest bank = gameOver guest ==> winner guest bank == Bank

-- | Given two hands, <+ puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
Empty <+ hand2 = hand2
(Add card hand) <+ hand2 = Add card (hand <+ hand2)


-- | Property: <+ should be associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- | Property: size of the combined hand should be the sum of the sizes
-- | of the two individual hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 =
    (size p1 + size p2) == size (p1 <+ p2)

-- | Given a suit, return all cards in that suit
sameSuit :: Suit -> Hand
sameSuit s = foldr Add Empty cards
    where
      cards = [Card r s | r <- [Numeric n | n <- [2..10]] ++
              [Jack, Queen, King, Ace]]

-- | Property: size of a hand with all cards in a suit is 13
prop_size_sameSuit :: Suit -> Bool
prop_size_sameSuit s = size (sameSuit s) == 13

-- | A function that returns a full deck of cards
fullDeck :: Hand
fullDeck = foldr (<+) Empty (map sameSuit suits)
  where suits = [Hearts, Spades, Diamonds, Clubs]

-- | Property: any "valid" card is a part of the full deck
prop_fullDeckFull :: Card -> Bool
prop_fullDeckFull (Card (Numeric n) s) =
      (n >=2 && n <= 10) == belongsTo (Card (Numeric n) s) fullDeck
prop_fullDeckFull c                    = belongsTo c fullDeck

-- | Property: a full deck contains 52 cards
prop_full :: Bool
prop_full = size fullDeck == 52

-- | Given a deck and a hand, draw one card from the deck and put on the hand.
-- | Return both the deck and the hand (in that order)
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, (Add card hand))

-- | Property: drawing a card preserves the total number of cards
prop_drawTotal :: Hand -> Hand -> Bool
prop_drawTotal d h | d /= Empty = size d + size h == size d' + size h'
                   | otherwise  = True
  where (d',h') = draw d h

-- | Given an empty hand return the bank's final hand
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- | Given the BlackJack rules gives us the best hand for the bank
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand >= 16 = bankHand
                        | otherwise            = playBank' deck' bankHand'
                       where (deck',bankHand') = draw deck bankHand

-- | Property: given a deck with value >= 16 the bank will score >= 16
prop_playBank16 :: Hand -> Bool
prop_playBank16 d | value d >= 16 = value (playBank d) >= 16
                  | otherwise     = True

-- | Removes the nth card from a hand
removedCard :: Integer -> Hand -> (Card, Hand)
removedCard 0 (Add card hand) = (card, hand)
removedCard n (Add card hand) = (card', Add card h')
  where (card', h') = removedCard (n-1) hand

-- | Returns True if a card belongs to a given deck
belongsTo :: Card -> Hand -> Bool
belongsTo c Empty      = False
belongsTo c (Add c' d) = c == c' || belongsTo c d

-- | Given a hand, returns a hand with same cards but shuffled
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g d     = Add c (shuffle g' d')
  where
    (n,g') = randomR (0,size d - 1) g
    (c,d') = removedCard n d

-- | Property: shuffling preserves deck size
prop_shuffleSize :: StdGen -> Hand -> Bool
prop_shuffleSize g d = size (shuffle g d) == size d

-- | Property: shuffling doesn't change the cards in the deck
prop_shuffleSameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffleSameCards g c d = belongsTo c d == belongsTo c (shuffle g d)
