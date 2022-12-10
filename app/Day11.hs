{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day11 (p1, p2) where

import Control.Lens (ix, makeLenses, (%~), (&), (.~))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (isDigit)
import Data.List.Extra (chunksOf, sortOn)
import Data.Ord (Down (..))
import Lib (if')
import Text.ParserCombinators.ReadP (ReadP, char, get, munch, readP_to_S, skipSpaces, string)
import Prelude hiding (round)

type Item = Int

data Monkey = Monkey
    { _number :: Int
    , _items :: [Item]
    , _operation :: Item -> Item
    , _throw :: Item -> Int
    }
$(makeLenses ''Monkey)

readOperation :: ReadP (Item -> Item)
readOperation = do
    c <- get
    skipSpaces
    n <- munch (/= '\n')
    let f' = case c of
            '+' -> (+)
            '*' -> (*)
            _ -> error "Invalid operation"
    pure $ case n of
        "old" -> \x -> f' x x
        k -> \x -> f' x (read k)

readMonkey :: ReadP (Monkey, Int)
readMonkey = do
    n <- nextNumber
    _ <- munch (not . isDigit)
    is <- (\x -> read $ "[" ++ x ++ "]") <$> munch (/= '\n')
    _ <- char '\n'
    skipSpaces
    _ <- string "Operation: new = old "
    op <- readOperation
    d <- nextNumber
    a <- nextNumber
    b <- nextNumber
    pure $ (,d) $ Monkey n is op (\x -> if x `mod` d == 0 then a else b)
  where
    nextNumber :: ReadP Int
    nextNumber = munch (not . isDigit) >> read <$> munch isDigit

readInput :: String -> [(Monkey, Int)]
readInput = map ((fst . head . readP_to_S readMonkey) . unlines) . chunksOf 7 . lines

turn :: (Int, Bool) -> Monkey -> (Monkey, [(Item, Int)])
turn (md, k) m@(Monkey _ (i : is) op to) =
    let i' = (`mod` md) $ if' k (`div` 3) $ op i
        t = to i'
     in second ((i', t) :) $ turn (md, k) (m & items .~ is)
turn _ m = (m, [])

round :: (Int, Bool) -> [Monkey] -> ([Monkey], [Int])
round k m =
    let is = [0 .. length m - 1]
     in foldl (\(ms, ns) i -> second (\n -> ns & ix i .~ n) $ round' i ms) (m, map (const 0) m) is
  where
    round' :: Int -> [Monkey] -> ([Monkey], Int)
    round' i ms =
        let (m', its) = turn k $ ms !! i
            ms' = foldl (\x (i', t) -> x & ix t . items %~ (i' :)) ms its
         in (ms' & ix i .~ m', length its)

f :: (Int, Bool) -> String -> String
f (n, k) s =
    let (ms, ds) = unzip $ readInput s
        modulo = product ds
     in show
            . product
            . take 2
            . sortOn Down
            . snd
            . (!! n)
            . iterate (\(ms', ns) -> second (zipWith (+) ns) $ round (modulo, k) ms')
            $ (ms, map (const 0) ms)

p1, p2 :: String -> String
p1 = f (20, True)
p2 = f (10000, False)
