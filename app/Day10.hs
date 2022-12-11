module Day10 (p1, p2) where

import Control.Monad ((>=>))
import Data.Char (toUpper)
import Data.List.Extra (chunksOf)

data Instruction = ADDX Int | NOOP deriving (Show, Read)

readInput :: String -> [Instruction]
readInput = map read . lines . map toUpper

clock :: Instruction -> Int -> [Int]
clock (ADDX n) x = [x + n, x]
clock NOOP x = [x]

f :: String -> [Int]
f = reverse . foldr ((. head) . clock >=> (++)) [1] . reverse . readInput

p1 :: String -> Int
p1 s = sum . map (\i -> i * f s !! pred i) $ [20, 60 .. 220]
p2 :: String -> String
p2 s =
    unlines
        . ([] :)
        . chunksOf 40
        . init
        . zipWith (\x p -> if abs (p - x) <= 1 then '#' else '.') (f s)
        . concat
        $ repeat [0 .. 39]
