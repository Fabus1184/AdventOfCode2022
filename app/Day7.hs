{-# LANGUAGE TupleSections #-}

module Day7 where

import Control.Monad.Trans.State (State, execState, get, modify, put)

import Data.Tuple.Extra (thd3)
import Prelude hiding (foldl)

data File = File
    { fileName :: String
    , fileSize :: Int
    }
    deriving (Show)

data Dir = Dir
    { dirName :: String
    , dirFiles :: [File]
    , dirDirs :: [Dir]
    }
    deriving (Show)

type Path = [String]

showDir :: Dir -> Int -> String
showDir d n =
    replicate (n * 2) ' '
        ++ "- "
        ++ dirName d
        ++ "\t(dir)"
        ++ "\n"
        ++ concatMap (`showDir` (n + 1)) (dirDirs d)
        ++ unlines (map ((replicate (2 * succ n) ' ' ++) . ("- " ++) . fileName) (dirFiles d))

insertFile :: Path -> File -> Dir -> Dir
insertFile = insertFile'
  where
    insertFile' :: Path -> File -> Dir -> Dir
    insertFile' [] f d = d{dirFiles = f : dirFiles d}
    insertFile' (x : xs) f d =
        d
            { dirDirs =
                case findDir x (dirDirs d) of
                    Nothing -> insertFile xs f (Dir x [] []) : dirDirs d
                    Just d' -> insertFile xs f d' : filter ((/= x) . dirName) (dirDirs d)
            }
    findDir :: String -> [Dir] -> Maybe Dir
    findDir _ [] = Nothing
    findDir x' (d' : ds) =
        if dirName d' == x'
            then Just d'
            else findDir x' ds

dirSize :: Dir -> Int
dirSize d = sum (map fileSize (dirFiles d)) + sum (map dirSize (dirDirs d))

dirs :: Dir -> [Dir]
dirs d = d : concatMap dirs (dirDirs d)

files :: Dir -> [File]
files d = dirFiles d ++ concatMap files (dirDirs d)

parse :: State (Path, [String], Dir) ()
parse = do
    (path, k, fs) <- get
    let (('$' : ' ' : cmd) : xs) = k
    case cmd of
        "cd .." -> put (init path, xs, fs)
        ('c' : 'd' : ' ' : dir') -> put (path ++ [dir'], xs, fs)
        "ls" -> do
            put (path, xs, fs)
            let entries = takeWhile ((/= '$') . head) xs
            let files' = filter (\e -> take 3 e /= "dir") entries
            mapM_
                ( \f ->
                    let (size, name') = break (== ' ') f
                     in modify (\(path', b, fs') -> (path', b, insertFile path' (File (tail name') (read size)) fs'))
                )
                files'
            modify $ \(_, b, fs') -> (path, drop (length entries) b, fs')
        _ -> error $ "Unknown command: " <> cmd
    (_, k', _) <- get
    case k' of
        [] -> pure ()
        _ -> parse

p1, p2 :: String -> String
p1 =
    show
        . sum
        . filter (<= 100000)
        . map dirSize
        . dirs
        . thd3
        . execState parse
        . ([],,Dir "/" [] [])
        . tail
        . lines
p2 s =
    let fs = thd3 . execState parse . ([],,Dir "/" [] []) . tail . lines $ s
        total = 70000000
        required = 30000000
        used = sum $ map fileSize $ files fs
        toFree = used - total + required
     in show $ minimum $ filter (>= toFree) $ map dirSize (dirs fs)