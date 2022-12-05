{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

import Control.Applicative (Applicative (liftA2))
import Control.Monad (when)
import Control.Monad.Extra (mapMaybeM)
import Data.Tuple.Extra (fst3, snd3)
import Formatting (formatToString, (%))
import Formatting.ShortFormatters (d, s)
import Language.Haskell.TH (integerL, listE, litE, lookupValueName, tupE, varE)
import System.Environment (getArgs)

solutions :: [(Int, Int, IO String)]
solutions =
    $( mapMaybeM
        (\(day, part) -> ((day,part,) <$>) <$> lookupValueName ("Day" <> show day <> ".p" <> show part))
        [(day, part) | day <- [1 :: Int .. 25], part <- [1 :: Int, 2]]
        >>= listE . map (\(day, n, x) -> tupE [litE (integerL (fromIntegral day)), litE (integerL (fromIntegral n)), varE x])
     )

main :: IO ()
main = do
    as <- map read <$> getArgs
    let solutions' =
            filter
                ( case as of
                    [day, part] -> (== (day, part)) . liftA2 (,) fst3 snd3
                    [day] -> (== day) . fst3
                    _ -> const True
                )
                solutions
    when (null solutions') $ error "No solutions"
    mapM_ (\(d', p', s') -> s' >>= putStrLn . formatToString ("Day" % d % " p" % d % ": " % s) d' p') solutions'
