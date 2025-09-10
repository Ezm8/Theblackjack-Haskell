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

module Blackjack where

-- Import necessary modules
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import Data.ByteString (count)
import GHC.CmmToAsm.Reg.Graph.Stats (RegAllocStats(raCoalesced))

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
sizeSteps = [size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades:[]))
            , 1 + size ( Card Jack Spades: [])
            , 1 + 1 + size ([])
            , 1 + 1 + 0 
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
value [] = 0 
--value n = sum [valueRank r | (Card r _) <- n]
value (x:xs) = if sum [valueCard x + value xs] < 1 then 
    
-- Task A4 --
--
gameOver :: Hand -> Bool
gameOver = undefined

winner :: Hand -> Hand -> Player
winner = undefined

--------------------------------------------------------------------------------
-- Part B
---------------------------------------------------------------------------------

-- Task B1 --
fullDeck :: Deck
fullDeck = undefined

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2 --

draw :: Deck -> Hand -> (Deck, Hand)
draw = undefined

-- Task B3 --

playBank :: Deck -> Hand
playBank = undefined

-- Task B4 --

pick :: Double -> Deck -> Card
pick = undefined

shuffle :: [Double] -> Deck -> Deck
shuffle = undefined

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
prop_size_shuffle (Rand randomlist) deck = undefined

-- Task B6 --

-- follow the instructions on Canvas

