{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Day02 (p1, p2) where

import Data.Biapplicative (biliftA2)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)

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
    = Win
    | Draw
    | Loss
    deriving (Eq, Show)

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

scores :: RPS -> RPS -> (Int, Int)
scores a b =
    let k = case myOutcome a b of
            Win -> (0, 6)
            Draw -> (3, 3)
            Loss -> (6, 0)
     in biliftA2 (+) (+) (both (succ . fromEnum) (a, b)) k

myScore :: [(RPS, RPS)] -> Int
myScore = sum . map (snd . uncurry scores)

input :: [(Char, a)] -> IO [(RPS, a)]
input s = map (\l -> (read [head l], fromJust $ lookup (l !! 2) s)) . lines <$> readFile "input02.txt"

p1 :: IO Int
p1 = myScore <$> input strategy1

p2 :: IO Int
p2 = myScore . map (\(m, o) -> (m,) $ findMove o m) <$> input strategy2