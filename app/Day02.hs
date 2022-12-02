{-# LANGUAGE InstanceSigs #-}

module Day02 (p1, p2) where

import Data.Maybe (fromJust)

data RPS
    = Rock
    | Paper
    | Scissors
    deriving (Eq, Show, Enum)

instance Read RPS where
    readsPrec :: Int -> ReadS RPS
    readsPrec _ "A" = [(Rock, "")]
    readsPrec _ "B" = [(Paper, "")]
    readsPrec _ "C" = [(Scissors, "")]
    readsPrec _ _ = []

data Outcome
    = Loss
    | Draw
    | Win
    deriving (Eq, Show, Enum)

strategy1 :: [(Char, RPS)]
strategy1 = [('X', Rock), ('Y', Paper), ('Z', Scissors)]

strategy2 :: [(Char, Outcome)]
strategy2 = [('X', Loss), ('Y', Draw), ('Z', Win)]

myOutcome :: RPS -> RPS -> Outcome
myOutcome Rock Paper = Win
myOutcome Rock Scissors = Loss
myOutcome Paper Rock = Loss
myOutcome Paper Scissors = Win
myOutcome Scissors Rock = Win
myOutcome Scissors Paper = Loss
myOutcome _ _ = Draw

findMove :: Outcome -> RPS -> RPS
findMove o r = head [m | m <- [Rock ..], myOutcome r m == o]

myScore :: RPS -> RPS -> Int
myScore a b = succ (fromEnum b) + (3 * fromEnum (myOutcome a b))

totalScore :: [(RPS, RPS)] -> Int
totalScore = sum . map (uncurry myScore)

input :: [(Char, a)] -> IO [(RPS, a)]
input s = map (\[a, _, b] -> (read [a], fromJust $ lookup b s)) . lines <$> readFile "input02.txt"

p1 :: IO Int
p1 = totalScore <$> input strategy1

p2 :: IO Int
p2 = totalScore . map (\(m, o) -> (m, findMove o m)) <$> input strategy2