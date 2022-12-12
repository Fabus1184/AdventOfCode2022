{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day12 (p1, p2) where

import Algorithm.Search (aStar)
import Control.Applicative (liftA3)
import Data.Char (ord)
import Data.Ix (inRange)
import Data.Maybe (fromJust)
import Lib (if', (<$$>))

data Position = Height Int | Start | End deriving (Show, Eq)

readInput :: String -> [[Position]]
readInput = ((\case 'S' -> Start; 'E' -> End; c -> Height (ord c - ord 'a')) <$$>) . lines

fields :: [[Position]] -> [(Position, (Int, Int))]
fields = concatMap (\(y, row) -> zipWith (\x -> (,(x, y))) [0 ..] row) . zip [0 ..]

neighbors :: Bool -> [[Position]] -> (Int, Int) -> [(Int, Int)]
neighbors k ps (x, y) =
    filter (\(x', y') -> if' k negate (height (ps !! y' !! x') - height (ps !! y !! x)) <= 1)
        . filter inBounds
        $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    height :: Position -> Int
    height = \case
        Height h -> h
        Start -> 0
        End -> 26
    inBounds :: (Int, Int) -> Bool
    inBounds (x', y') = inRange (0, length ps - 1) y' && inRange (0, length (head ps) - 1) x'

end :: [[Position]] -> (Int, Int)
end = snd . head . filter ((== End) . fst) . fields

start :: [[Position]] -> (Int, Int)
start = snd . head . filter ((== Start) . fst) . fields

p1, p2 :: String -> Int
p1 =
    fst
        . fromJust
        . liftA3 (\ns e s -> aStar ns (\_ _ -> 1) (const 0) (== e) s) (neighbors False) end start
        . readInput
p2 =
    fst
        . fromJust
        . liftA3
            (\ns e p -> aStar ns (\_ _ -> 1) (const 0) p e)
            (neighbors True)
            end
            (\ps (x, y) -> ps !! y !! x `elem` [Start, Height 0])
        . readInput