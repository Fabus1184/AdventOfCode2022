{-# LANGUAGE LambdaCase #-}

module Day12 (p1, p2) where

import Algorithm.Search (aStar, dijkstra)
import Control.Applicative (liftA2, liftA3)
import Data.Char (ord)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (uncurry3)
import Lib (if')

data Position = Position
    { position :: (Int, Int)
    , height :: Int
    , isStart :: Bool
    , isEnd :: Bool
    }
    deriving (Show, Eq, Ord)

readInput :: String -> [Position]
readInput = concat . zipWith (\y l -> zipWith (\x -> uncurry3 (Position (x, y)) . \case 'S' -> (,,) 0 True False; 'E' -> (,,) 25 False True; c -> (,,) (ord c - ord 'a') False False) [0 ..] l) [0 ..] . lines

neighbors :: Bool -> [Position] -> Position -> [Position]
neighbors k ps p = filter (\p' -> if' k negate (height p' - height p) <= 1 && distance (position p) (position p') == 1) ps

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

p1, p2 :: String -> Int
p1 =
    fst
        . fromJust
        . liftA3 (\ns e -> aStar ns (\_ _ -> 1) (distance (position e) . position) (== e)) (neighbors False) (fromJust . find isEnd) (fromJust . find isStart)
        . readInput
p2 =
    fst
        . fromJust
        . liftA2 (\ns e -> dijkstra ns (\_ _ -> 1) ((== 0) . height) e) (neighbors True) (fromJust . find isEnd)
        . readInput