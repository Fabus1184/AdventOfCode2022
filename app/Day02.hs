{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Day02 (p1, p2) where

import Data.Maybe (fromJust)

data Move = Rock | Paper | Scissors deriving (Eq, Show, Enum)

instance Read Move where
    readsPrec :: Int -> ReadS Move
    readsPrec _ "A" = [(Rock, "")]
    readsPrec _ "B" = [(Paper, "")]
    readsPrec _ "C" = [(Scissors, "")]
    readsPrec _ _ = []

data Outcome = Loss | Draw | Win deriving (Eq, Show, Enum)

myOutcome :: Move -> Move -> Outcome
myOutcome Rock Paper = Win
myOutcome Rock Scissors = Loss
myOutcome Paper Rock = Loss
myOutcome Paper Scissors = Win
myOutcome Scissors Rock = Win
myOutcome Scissors Paper = Loss
myOutcome _ _ = Draw

totalScore :: [(Move, Move)] -> Int
totalScore = sum . map (\(a, b) -> succ (fromEnum b) + (3 * fromEnum (myOutcome a b)))

input :: [(Char, a)] -> IO [(Move, a)]
input s = map (\[a, _, b] -> (read [a], fromJust $ lookup b s)) . lines <$> readFile "input02.txt"

p1 :: IO Int
p1 = totalScore <$> input [('X', Rock), ('Y', Paper), ('Z', Scissors)]

p2 :: IO Int
p2 = totalScore . map (\(m, o) -> (m,) $ head [x | x <- [Rock ..], myOutcome m x == o]) <$> input [('X', Loss), ('Y', Draw), ('Z', Win)]