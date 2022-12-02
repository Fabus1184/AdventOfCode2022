module Main (main) where

import qualified Day02 (p1, p2)

main :: IO ()
main = do
    Day02.p1 >>= print
    Day02.p2 >>= print
