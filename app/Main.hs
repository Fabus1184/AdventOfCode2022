{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

import Advent (AoC (AoCInput, AoCSubmit), defaultAoCOpts, mkDay_, runAoC)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Lens ((^.), _1)
import Control.Monad.Extra (liftM4, mapMaybeM, when)
import Data.Text (unpack)
import Formatting (formatToString, (%))
import Formatting.ShortFormatters (s)
import Language.Haskell.TH (appE, listE, lookupValueName, stringE, tupE, varE)
import Language.Haskell.TH.Syntax (showName)
import Lib (MyShow (myShow), Showable (..), tmap4, ttake2, untup2, untup4)
import System.Environment (getArgs)
import System.TimeIt (timeIt)

solutions :: [(Int, Int, String, String -> Showable)]
solutions =
    $( mapMaybeM
        ( \(a, b, c) ->
            lookupValueName c >>= \case
                Nothing -> pure Nothing
                Just n -> Just <$> liftM4 (,,,) [|a|] [|b|] (stringE $ showName n) (varE n)
        )
        [(day, part, "Day" <> show day <> "." <> "p" <> show part) | day <- [1 .. 25] :: [Int], part <- [1, 2] :: [Int]]
        >>= listE . map (tupE . untup4 . tmap4 (pure, pure, pure, appE [|(MkShowable .)|] . pure))
     )

main :: IO ()
main = do
    Just sk <- lookup "SESSION_KEY" <$> loadFile defaultConfig
    as <- map read <$> getArgs :: IO [Int]
    let opts = defaultAoCOpts 2022 sk
    let solutions' =
            filter
                ( case as of
                    [day] -> (== day) . (^. _1)
                    [_, _] -> (== as) . untup2 . ttake2
                    _ -> const True
                )
                solutions
    when (null solutions') $ error "No solutions"
    mapM_
        ( \(day, part, name, solution) -> do
            res <- either (error . show) unpack <$> (runAoC opts . AoCInput . mkDay_ . fromIntegral $ day)
            timeIt $ putStrLn $ formatToString (s % ": " % s) name (myShow $ solution res)
            either (error . show) ((>>) (putStr "=> ") . print . snd) =<< runAoC opts (AoCSubmit (mkDay_ $ fromIntegral day) (toEnum $ pred part) (myShow $ solution res))
        )
        solutions'
