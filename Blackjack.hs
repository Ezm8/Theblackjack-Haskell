{- |
Module      : Lab2
Description : Skeleton for lab 2: Blackjack
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <list your names here>
Lab group   : <group number>
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}


module Blackjack where

import Prelude hiding (interleavem, Some)
-- Import necessary modules
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import Data.ByteString (count)
import GHC.CmmToAsm.Reg.Graph.Stats (RegAllocStats(raCoalesced))
import GHC.CmmToAsm.AArch64.Instr (x1)
import Test.QuickCheck.Gen (Gen(unGen))
import Data.List (delete)

-- Task A1 --

hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

{-
size hand2
  = size(Card(numeric 2 ) Heats: (Card Jack Spades : []))
  = 1 + size ((Card Jack Spades: []))
  = 1 + 1 + size([])
  = 1 + 1 + 0 
  = 2 
-}


sizeSteps :: [Int]

sizeSteps = [
              size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades: ([])))
            , 1 + size ( Card Jack Spades:([]))
            , 1 + 1 + size ([])
            , 1 + 1 + 0 
            , 2 + 0
            , 2
            ]


-- Task A2 --

display :: Hand -> String
display [] = ""
display ((Card (Numeric k) s):[]) = show k ++ " of " ++ show s 
display ((Card k s):[])           = show k ++ " of " ++ show s 
display ((Card (Numeric n) s):xs) = show n ++ " of " ++ show s ++ ", " ++ display xs 
display ((Card k s):xs)           = show k ++ " of " ++ show s ++ ", "  ++ display xs 


display1 :: Hand -> String
display1 [] = ""
display1 ((Card k s):[]) = case k of
  (Numeric c) -> show c ++ " of " ++ show s
  t           -> show t ++ " of " ++ show s
display1 (x:xs) = display1 [x] ++ ", " ++ display1 xs

-- Task A3 --

valueRank:: Rank -> Int
valueRank k =  case k of 
  (Numeric c) -> c
  Ace         -> 11
  t           -> 10

valueCard:: Card -> Int
valueCard (Card r _) = valueRank r 
--valueCard k = valueRank (rank k)


numberOfAces:: Hand -> Int
numberOfAces n = length [r | (Card r _) <- n, r == Ace ]   
--numberOfAces n = length [x | x <- n, rank x == Ace]
--numberOfAces (x:xs) = if rank x == Ace then 1 + numberOfAces xs else 0 + numberOfAces xs

--numberOften :: [Int] -> Int
--numberOften n =  length [x | x <- n, x == 10]


value :: Hand -> Int
value n 
  |sumHand <= 21 = sumHand
  |otherwise = sumHand - (numberOfAces n * 10)
  where 
    sumHand =  sum [valueRank r | (Card r _) <- n]

  -- |otherwise = sum [valueRank r | (Card r _) <- n,  r /= Ace] + 
  --              sum [valueRank r - 10 | (Card r _) <- n,  r == Ace]


hand3 :: Hand
hand3 = [Card (Numeric 10) Hearts, Card (Numeric 2) Spades, Card (Numeric 10) Hearts]


-- Task A4 --
--
gameOver :: Hand -> Bool
gameOver n 
  |value n > 21 = True
  |otherwise = False


winner :: Hand -> Hand -> Player
winner n c 
  |value n > value c || value n == 21 || value c > 21 = Guest
  |otherwise = Bank


--------------------------------------------------------------------------------
-- Part B

---------------------------------------------------------------------------------

-- Task B1 -- 
fullDeck :: Deck -- --> [Card] -> Card Rank Suit -> Card r s 
fullDeck = [Card r s |r <- rank', s <- suit']
    where 
    rank' :: [Rank]
    rank' = [Numeric 2, Numeric 3, Numeric 4, 
            Numeric 5, Numeric 6, Numeric 7, Numeric 8, Numeric 9, Numeric 10, Jack, Queen, King, Ace] 
    suit' ::[Suit]
    suit' = [Hearts, Spades, Diamonds, Clubs]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2 --


draw :: Deck -> Hand -> (Deck, Hand)
draw [] hand = error " empty deck"
draw (x:xs) hand = (xs, x:hand)



-- detta är hur playbank kör 
-- The bank draws cards until its score is 16 or higher, and then it stops.
playBank :: Deck -> Hand
playBank xs  = playBank' xs []
  where 
    playBank' :: Deck -> Hand -> Hand
    playBank' deck bankHand
      |value bankHand < 16 = playBank' deck' bankHand'
      |otherwise  = bankHand
        where 
            (deck',bankHand') = draw deck bankHand
            --detta är ett sätt att deconstructa tuple 


{-playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand 
  |value bankHand < 16 = uncurry playBank' result
  |otherwise  = bankHand
    where 
      result = draw deck bankHand
      (x,y)  = draw deck bankHand
-}

 


-- Task B4 --

pick :: Double -> Deck -> Card
pick 1 deck = last deck
pick x deck = deck !! floor (converting x deck)
  where
    converting x xs =  x  *  fromIntegral (length xs)



shuffle :: [Double] -> Deck -> Deck 
shuffle _ [] = []      
shuffle (x:xs) deck = 
    let card = pick x deck; remainingdeck = delete card deck
    in  card: shuffle xs remainingdeck

--result n xs = delete (pick n xs)  
--result hämta en kort och ta kort den kortet från originalen, return deck som är bortagen kort 


runShuffle :: IO Deck
runShuffle = do
  Rand ds <- generate arbitrary
  return (shuffle ds fullDeck)

-- Task B5 --

belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) = 
  card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = 
  length (shuffle randomlist deck)== length deck


-- Task B6 --

-- follow the instructions on Canvas

implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }


main :: IO ()
main = runGame implementation

