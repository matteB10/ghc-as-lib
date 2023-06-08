{-# LANGUAGE GADTs #-}
module BlackJack where

-- All code using functions from QuickCheck of Random is commented out
--import Test.QuickCheck
--import System.Random
import Data.List
-- import Data.Char (toLower)

-- | A card has a rank and belongs to a suit.
data Card = Card Rank Suit
      deriving (Eq, Show)

-- | A rank is either a numeric card, a face card, or an ace. The
-- numeric cards range from two to ten.
data Rank = Numeric Integer | Jack | Queen | King | Ace
           deriving (Eq, Show)

-- | All the different suits.
data Suit = Hearts | Spades | Diamonds | Clubs
           deriving (Eq, Show)

-- | A hand of cards. This data type can also be used to represent a
-- deck of cards.
data Hand = Empty | Add Card Hand
           deriving (Eq, Show)

 -- | rank and suit give the respective parts of a card
rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

-- | The size of a hand.
size :: Num a => Hand -> a
size Empty            = 0
size (Add card hand)  = 1 + size hand
    ------------------- 2A --------------------


     -- | Used to display a hand as strings
display :: Hand -> String
display Empty = ""
display (Add card hand)
     | size hand == 0 = displayCard card ++ "\n"
     | otherwise      = displayCard card ++ "\n" ++ display hand



-- | Used for testing that display function behave as intended.
prop_display :: Hand -> Bool
prop_display Empty = display Empty == ""
prop_display (Add _ hand) = and [displayCard x `isInfixOf` display hand | x <- [Card (Numeric 2) Hearts]]

 -- | Converts a hand into list of cards representing the hand.
handAsCardList :: Hand -> [Card]
handAsCardList Empty = []
handAsCardList (Add card hand)
        | size hand == 0    = [card]
        | otherwise         = handAsCardList hand    


-- | Used to display a card as a string
displayCard :: Card -> String
displayCard (Card (Numeric i) suit) = show i ++ " of " ++ show suit
displayCard (Card rank suit)        = show rank ++ " of " ++ show suit 


-- constant
maxValue = 21

-- | get the value of a given hand
value :: Hand -> Integer
value hand = if value <= maxValue
                 then value
                 else value - (numberOfAces hand * 10)
        where value = initialValue hand

-- | get the initial value of a given hand
initialValue :: Hand -> Integer
initialValue Empty           = 0
initialValue (Add card hand) = valueRank (rank card) + initialValue hand

-- | get the value of given rank
valueRank :: Rank -> Integer
valueRank Ace         = 11
valueRank (Numeric i) = i
valueRank _           = 10


-- | Calculates number of Aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty           = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | otherwise        = numberOfAces hand

-- | Checks if the users score is > 21, used to decide if a user has lost the game
gameOver :: Hand -> Bool
gameOver hand = value hand > maxValue 

 -- | Used to decide if player or bank has won (order dependent)
winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest           = Bank
                  | gameOver bank            = Guest
                  | value guest > value bank = Guest
                  | otherwise                = Bank

-- | A type of players.
data Player = Guest | Bank
              deriving (Show, Eq) 


------------------ 2B ------------------------



-- | Given two hands, put the first one on top of the other
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand = hand
(<+) hand Empty = hand
(<+) (Add card hand1) hand2 = Add card (hand1 <+ hand2)


 -- | Used to test associativity of '<+' operator
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3



-- | Used to test that size of two combined hands equals the sum of the size of the individual hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = (size hand1 + size hand2) == size (hand1 <+ hand2)



-- | Returns a full deck
fullDeck :: Hand
fullDeck = foldr Add Empty cards
    where suits = [Hearts, Clubs, Diamonds, Spades]
          ranks = [Numeric i | i <- [2..10]] ++ [r | r <- [Jack, Queen, King, Ace]]
          cards = [Card r s | r <- ranks, s <- suits] 


-- | Given a deck and a hand, draw a card from the deck and put it on the hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "Can not draw a card from an empty deck!"
draw (Add card deck) hand = (deck, Add card hand)


-- | Used to play a bank round
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

-- | Helper function to draw as many card as needed when bank is playing
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand = if value biggerHand >= 16 then biggerHand else playBankHelper smallerDeck biggerHand
     where (smallerDeck,biggerHand) = draw deck hand
  
 
{- -- | Used to shuffle the deck
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty   = Empty
shuffleDeck rand deck = Add pickedCard (shuffleDeck rand' newDeck)
     where (n, rand') = randomR (1, size deck) rand
           (pickedCard, newDeck) = (pick n deck, removeCard n deck) -}


-- | Given a number, pick and return that card from the given hand
pick :: Int -> Hand -> Card
pick _ Empty                             = error "Can not pick a card from an empty hand"
pick n (Add card deck) | size deck == 0  = card
                       | n == 1          = card             -- 1 is first card in a hand
                       | otherwise       = pick (n-1) deck


-- | Given a number, remove that card from the hand and return the deck
removeCard :: Int -> Hand -> Hand
removeCard _ Empty                               = error "Can not remove a card from an empty deck"
removeCard n (Add card deck)  | n < 1 || n > 52  = Empty -- Can not remove a card that is not in the deck
                              | n == 1           = deck   -- 1 is first card in a hand
                              | otherwise        = Add card (removeCard (n-1) deck)
        where savedHand = Empty


{- -- | Test if all cards is still in deck after shuffle
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h -}

-- | Helper function used to check if a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

{- -- | Used to check if size of a hand is preserved after shuffle
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle rand deck = size deck == size (shuffleDeck rand deck)
 -}
list = [1..10]

list' = list \\ [list !! 0]    
-- | The interface to the students' implementation.

data Interface = Interface
  { iFullDeck :: Hand
  , iValue    :: Hand -> Integer
  , iDisplay  :: Hand -> String
  , iGameOver :: Hand -> Bool
  , iWinner   :: Hand -> Hand -> Player
  , iDraw     :: Hand -> Hand -> (Hand, Hand)
  , iPlayBank :: Hand -> Hand
  } 
implementation = Interface {
  iFullDeck = fullDeck,
  iValue = value,
  iDisplay = display,
  iGameOver = const True,
  iWinner = winner,
  iDraw = draw,
  iPlayBank = playBank 
}  
{- 
main :: IO ()
main = runGame implementation

-- | Runs a game given an implementation of the interface.
runGame :: Interface -> IO ()
runGame i =
  do putStrLn "Welcome to the game."
     let g = 1 
     gameLoop i (iShuffle i g (iFullDeck i)) Empty


-- | Play until the guest player is bust or chooses to stop.
gameLoop :: Interface -> Hand -> Hand -> IO ()
gameLoop i deck guest = do
     putStrLn $ "Your current score: " ++ displayHand i guest ++ "\n"
     if iGameOver i guest
       then finish i deck guest Empty
       else do putStr ("Draw "
                       ++ (if guest == Empty then "a " else "another ")
                       ++ "card? [y] ")
               yn <- getLine
               if null yn || not (map toLower yn == "n")
                 then do let (deck', guest') = iDraw i deck guest
                         gameLoop i deck' guest'
                 else do
                    putStrLn "\n"
                    bank <- playBank1 i deck
                    finish i deck guest bank

-}
-- | Display the bank's final score and the winner.
finish :: Interface -> Hand -> Hand -> Hand -> IO ()
finish i deck guest bank = do
  putStrLn $ "Your final score: " ++ displayHand i guest
  putStrLn $ "The bank's final score: " ++ displayHand i bank
  putStrLn $ "Winner: " ++ show (iWinner i guest bank)
   --where
    -- bank = iPlayBank i deck


-- | Used to play a bank round
playBank1 :: Interface -> Hand -> IO Hand
playBank1 i deck = playBankHelper1 i deck Empty

    -- | Helper function to draw as many card as needed when bank is playing
playBankHelper1 :: Interface -> Hand -> Hand -> IO Hand
playBankHelper1 i deck hand | iValue i hand >= 16  = return hand
                            | otherwise = do
                                let (deck', hand') = iDraw i deck hand
                                putStrLn ("Banks current score: " ++ displayHand i hand' ++ "\n") >> playBankHelper1 i deck' hand'
            --if iValue i hand' >= 16
            --then putStrLn ("Banks current score :" ++ displayHand i hand' ++ "\n") >> return hand'
            --else playBankHelper1 i deck' hand'

displayHand :: Interface -> Hand -> String
displayHand i hand = show (iValue i hand)
                  ++ if hand == Empty then ""
                     else " with cards: " ++ iDisplay i hand  