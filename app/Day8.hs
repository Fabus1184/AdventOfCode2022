module Day8 (p1, p2) where

import Control.Applicative (liftA2)

readInput :: String -> [[Int]]
readInput = map (map (read . (: []))) . lines

directions :: [[Int]] -> (Int, Int) -> [[Int]]
directions s (row, col) =
    let row' = s !! row
        col' = map (!! col) s
     in [ take col row'
        , reverse $ drop (succ col) row'
        , take row col'
        , reverse $ drop (succ row) col'
        ]

visible :: [[Int]] -> (Int, Int) -> Bool
visible = curry $ liftA2 (\p -> any (all (< p))) (\(s, (row, col)) -> s !! row !! col) (uncurry directions)

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore s (row, col) = product . map (vis . reverse) $ directions s (row, col)
  where
    vis [] = 0
    vis (x : xs)
        | x < s !! row !! col = 1 + vis xs
        | otherwise = 1

p1, p2 :: String -> String
p1 s =
    let s' = readInput s
        k =
            length $
                filter
                    id
                    [ visible s' (row, col)
                    | row <- [0 .. length s' - 1]
                    , col <- [0 .. length (head s') - 1]
                    ]
     in show k
p2 s =
    let s' = readInput s
        k =
            [ scenicScore s' (row, col)
            | row <- [0 .. length s' - 1]
            , col <- [0 .. length (head s') - 1]
            ]
     in show $ maximum k