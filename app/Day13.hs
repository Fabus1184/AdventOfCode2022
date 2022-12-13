{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Day13 (p1, p2) where

import Data.Char (isDigit)
import Data.List.Extra (chunksOf, sort)
import Data.List.Index (imap)
import Data.Tuple.Extra (both)
import Lib (tup2, untup2)
import Text.ParserCombinators.ReadP (ReadP, char, get, munch, readP_to_S, sepBy)

data Packet = List [Packet] | Value Int deriving (Show, Eq)

instance Ord Packet where
    compare :: Packet -> Packet -> Ordering
    compare (Value a) (Value b) = compare a b
    compare (List as) (List bs) = compare as bs
    compare (Value a) (List bs) = compare [Value a] bs
    compare (List as) (Value b) = compare as [Value b]

instance Read Packet where
    readsPrec :: Int -> ReadS Packet
    readsPrec _ = readP_to_S readPacket

readInput :: String -> [(Packet, Packet)]
readInput = map (both read . tup2) . chunksOf 2 . filter (not . null) . lines

readPacket :: ReadP Packet
readPacket =
    get >>= \case
        '[' -> List <$> sepBy readPacket (char ',') <* char ']'
        c -> Value . read . (c :) <$> munch isDigit

p1, p2 :: String -> Int
p1 = sum . imap (\i (a, b) -> if a < b then i + 1 else 0) . readInput
p2 =
    product
        . imap (\i p -> if p `elem` untup2 ds then i + 1 else 1)
        . sort
        . concatMap untup2
        . (ds :)
        . readInput
  where
    ds = (read "[[2]]", read "[[6]]")
