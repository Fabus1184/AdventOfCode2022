{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Day1
import qualified Day2
import qualified Day3

import Control.Monad.Extra (mapMaybeM)
import Language.Haskell.TH (listE, lookupValueName, stringE, tupE, varE)
import Language.Haskell.TH.Syntax (showName)

solutions :: [(String, IO Int)]
solutions =
    $( mapMaybeM
        (\(d, n) -> lookupValueName ("Day" <> show d <> ".p" <> show n))
        [(d, n) | d <- [1 :: Int .. 25], n <- [1 :: Int, 2]]
        >>= listE . map (\x -> tupE [stringE (showName x), varE x])
     )

main :: IO ()
main = do
    mapM_ (\(n, s) -> s >>= putStrLn . ((n <> ": ") <>) . show) solutions
