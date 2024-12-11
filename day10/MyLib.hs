module MyLib (part12) where

import Debug.Trace
import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.HashMap.Internal.Strict (HashMap)

-- Given a function, map that function over a list, but also pass the index of the element to the function.
mapIdx :: (Int -> a -> b) -> [a] -> [b]
mapIdx f = zipWith f [0..]

-- Given text file containing a matrix of chars, read into a list of (x, y, c) tuples and width/height
readMatrix :: String -> ([(Int, Int, Char)], Int, Int)
readMatrix contents = (concat $ mapIdx (\y line -> mapIdx (\x c -> (x, y, c)) line) lines, sizex, sizey)
    where
        lines = splitOn "\n" contents
        sizex = length $ head lines
        sizey = length lines

intMatrix :: [(Int, Int, Char)] -> [(Int, Int, Int)]
intMatrix = map (\(x, y, c) -> (x, y, read [c])) . filter (\(_, _, c) -> c `elem` ['0'..'9'])

-- Given a matrix, return all coordinates where val is 0
findStarts :: [(Int, Int, Int)] -> [(Int, Int)]
findStarts matrix = map (\(x, y, _) -> (x, y)) $ filter (\(_, _, c) -> c == 0) matrix

numAt :: (Int, Int) -> HashMap.HashMap (Int, Int) Int -> Int
numAt (x, y) matrix = 
    fromMaybe (-1) c
    where c = HashMap.lookup (x, y) matrix

-- Given a map and a starting point, find all the 9s we can reach from it.
score :: HashMap.HashMap (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
score matrix start = nub $ score2 matrix start 0

-- w/ next value to look for
score2 :: HashMap.HashMap (Int, Int) Int -> (Int, Int) -> Int -> [(Int, Int)]
score2 matrix pos 9 =
    if numAt pos matrix == 9 then [pos] else []
score2 matrix (x, y) val =
    if numAt (x, y) matrix == val then
        score2 matrix up (val + 1) ++ score2 matrix down (val + 1) ++ score2 matrix left (val + 1) ++ score2 matrix right (val + 1)
    else []
    where
        up = (x, y - 1)
        down = (x, y + 1)
        left = (x - 1, y)
        right = (x + 1, y)

-- keep track of the actual distinct paths we can take to each 9 from a starting point
rating :: HashMap.HashMap (Int, Int) Int -> (Int, Int) -> [[(Int, Int)]]
rating matrix start = rating2 matrix start 0 []

rating2 :: HashMap.HashMap (Int, Int) Int -> (Int, Int) -> Int -> [(Int, Int)] -> [[(Int, Int)]]
rating2 matrix pos 9 path =
    if numAt pos matrix == 9 then [path] else []
rating2 matrix (x, y) val path =
    if numAt (x, y) matrix == val then
        rating2 matrix up (val + 1) ((x,y):path) ++ rating2 matrix down (val + 1) ((x,y):path) ++ rating2 matrix left (val + 1) ((x,y):path) ++ rating2 matrix right (val + 1) ((x,y):path)
    else []
    where
        up = (x, y - 1)
        down = (x, y + 1)
        left = (x - 1, y)
        right = (x + 1, y)


-- get last element of a 3-tuple
trd :: (a, b, c) -> c
trd (_, _, c) = c

part12 :: IO ()
part12 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let (charmap, sizex, sizey) = readMatrix contents
    let matrix = intMatrix charmap

    let starts = findStarts matrix

    let matrixMap = HashMap.fromList $ map (\(x, y, c) -> ((x, y), c)) matrix
    let scores = concat $ map (\st -> score matrixMap st) starts
    putStrLn ("Part 1: " ++ show (length scores))

    let ratings = concat $ map (\st -> rating matrixMap st) starts

    putStrLn ("Part 2: " ++ show (length ratings))
