module Day8 (p1, p2) where

import Control.Applicative (liftA2)
import Control.Lens (ix, (^.))
import Data.Functor ((<&>))
import Lib ((<$$>))

readInput :: String -> [[Int]]
readInput = (read . pure <$$>) . lines

directions :: (Int, Int) -> [[Int]] -> [[Int]]
directions (row, col) s =
    let row' = s !! row
        col' = map (!! col) s
     in [ take col row'
        , reverse $ drop (succ col) row'
        , take row col'
        , reverse $ drop (succ row) col'
        ]

visible :: (Int, Int) -> [[Int]] -> Bool
visible (row, col) = (^. ix row . ix col) >>= (directions (row, col) <&>) . any . all . (>)

scenicScore :: (Int, Int) -> [[Int]] -> Int
scenicScore (row, col) = (^. ix row . ix col) >>= \x -> product . map (foldr (\k -> if k < x then succ else const 1) 0 . reverse) . directions (row, col)

ps :: (Enum a, Num a) => a -> [(a, a)]
ps n = [(row, col) | row <- [0 .. pred n], col <- [0 .. pred n]]

p1, p2 :: String -> String
p1 = show . liftA2 ((length .) . filter) (flip visible) (ps . length) . readInput
p2 = show . maximum . liftA2 map (flip scenicScore) (ps . length) . readInput