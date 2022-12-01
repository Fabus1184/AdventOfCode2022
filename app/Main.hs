module Main (main) where

import qualified Day01 (p1, p2)

main :: IO ()
main = do
    Day01.p1 >>= print
    Day01.p2 >>= print
