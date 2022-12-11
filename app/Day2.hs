{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day2 (p1, p2) where

import Data.Maybe (fromJust)

data Move = Rock | Paper | Scissors deriving (Eq, Show, Enum)

instance Read Move where
    readsPrec :: Int -> ReadS Move
    readsPrec _ =
        (: []) . (,"") . \case
            "A" -> Rock
            "B" -> Paper
            "C" -> Scissors
            _ -> undefined

data Outcome = Loss | Draw | Win deriving (Eq, Show, Enum)

myOutcome :: Move -> Move -> Outcome
myOutcome a b
    | a == b = Draw
    | succ (fromEnum a) `mod` 3 == fromEnum b = Win
    | otherwise = Loss

totalScore :: [(Move, Move)] -> Int
totalScore = sum . map (\(a, b) -> succ (fromEnum b) + (3 * fromEnum (myOutcome a b)))

readInput :: [(Char, a)] -> String -> [(Move, a)]
readInput s = map (\[a, _, b] -> (read [a], fromJust $ lookup b s)) . lines

p1, p2 :: String -> Int
p1 = totalScore <$> readInput [('X', Rock), ('Y', Paper), ('Z', Scissors)]
p2 = totalScore . map (\(m, o) -> (m,) $ head [x | x <- [Rock ..], myOutcome m x == o]) <$> readInput [('X', Loss), ('Y', Draw), ('Z', Win)]