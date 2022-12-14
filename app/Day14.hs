{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Day14 (p1, p2) where

import Control.Lens (makeLenses, (%~), (.~))
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Ix (Ix (inRange))
import Data.List (sortOn)
import Data.List.Extra (maximumOn)
import Data.Ord (Down (..))
import GHC.IO (unsafePerformIO)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S, sepBy, string)

type Position = (Int, Int)
data Sand = Sand
    { _position :: Position
    , _rest :: Bool
    }
    deriving (Show, Eq)
$(makeLenses ''Sand)

newtype Path = Path
    { _path :: [Position]
    }
    deriving (Show, Eq)
$(makeLenses ''Path)

data Cave = Cave
    { _rocks :: [Path]
    , _sand :: [Sand]
    }
    deriving (Eq)
$(makeLenses ''Cave)

instance Show Cave where
    show :: Cave -> String
    show (Cave rocks sand) =
        let (minx, maxx, miny, maxy) = (450, 550, 0, 175)
         in unlines
                [ show y
                    ++ [ case (x, y) of
                        (500, 0) -> '+'
                        _ -> if any (isInPath (x, y) . _path) rocks then '#' else if (x, y) `elem` map _position sand then 'o' else '.'
                       | x <- [minx .. maxx]
                       ]
                | y <- [miny .. maxy]
                ]

instance Read Path where
    readsPrec :: Int -> ReadS Path
    readsPrec _ = readP_to_S readPath

readPosition :: ReadP Position
readPosition = do
    a <- read <$> munch1 isDigit
    _ <- char ','
    b <- read <$> munch1 isDigit
    pure (a, b)

readPath :: ReadP Path
readPath = Path <$> sepBy readPosition (string " -> ")

isInPath :: Position -> [Position] -> Bool
isInPath _ [] = False
isInPath p [p'] = p == p'
isInPath p@(px, py) ((x1, y1) : (x2, y2) : ps) =
    (y1 == y2 && py == y1 && inRange (min x1 x2, max x1 x2) px)
        || (x1 == x2 && px == x1 && inRange (min y1 y2, max y1 y2) py)
        || isInPath p ((x2, y2) : ps)

readInput :: String -> Cave
readInput = (`Cave` []) . map read . lines

stepSand :: Cave -> Cave
stepSand (Cave rocks sand) =
    Cave rocks
        $ foldl
            ( \sands s ->
                ( if _rest s
                    then s
                    else
                        let s' = f s sands
                         in if s' == s then s & rest .~ True else s'
                )
                    : sands
            )
            []
        $ sortOn (Down . snd . _position) (Sand (500, 0) False : sand)
  where
    f (Sand (x, y) _) sands
        | isValid (x, y + 1) = Sand (x, y + 1) False
        | isValid (x - 1, y + 1) = Sand (x - 1, y + 1) False
        | isValid (x + 1, y + 1) = Sand (x + 1, y + 1) False
        | otherwise = Sand (x, y) False
      where
        isSand p = p `elem` map _position sands
        isRock p = any (isInPath p . _path) rocks
        isValid p = not (isRock p) && not (isSand p)

isInAbyss :: Int -> Sand -> Bool
isInAbyss _ (Sand _ True) = False
isInAbyss n (Sand (_, y) False) = y > n

p1, p2 :: String -> Int
p1 s =
    let s' = readInput s
        abyss = snd $ maximumOn snd $ concatMap _path $ _rocks s'
        end = last $ takeWhile (not . any (isInAbyss abyss) . _sand) $ iterate stepSand s'
     in length $ filter _rest $ _sand end
p2 s =
    let s' = readInput s
        abyss = (2 +) $ snd $ maximumOn snd $ concatMap _path $ _rocks s'
        s'' = s' & rocks %~ (Path [(-10000, abyss), (10000, abyss)] :)
        end = head $ dropWhile ((Sand (500, 0) True `notElem`) . _sand) $ iterate stepSand s''
     in length $ filter _rest $ _sand end