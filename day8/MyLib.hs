module MyLib (part12) where

import Debug.Trace
import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable

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

trd :: (a, b, c) -> c
trd (_, _, c) = c

-- Given two antennas, return the two points that are the antinodes
-- note these may go outside the bounds of the original matrix.
-- antinodes a b not the same as antinode b a. Call them both ways.
antinodes :: (Int, Int, Char) -> (Int, Int, Char) -> [(Int, Int)]
antinodes (x1, y1, c) (x2, y2, _) =
    -- trace ("antinodes: " ++ show c ++ ": " ++ show (x1, y1) ++ " " ++ show (x2, y2) ++ " at (" ++ show newX1 ++ ", " ++ show newY1 ++ ") and (" ++ show newX2 ++ ", " ++ show newY2 ++ ")") $
    [(newX1, newY1)]
    where diffX = x1 - x2
          diffY = y1 - y2
          -- extrapolate the antinodes
          newX1 = x1 + diffX
          newY1 = y1 + diffY

-- Also include original antenna locations in the antinode list, and keep extrapolating.
antinodes2 :: (Int, Int, Char) -> (Int, Int, Char) -> Int -> Int -> [(Int, Int)]
antinodes2 (x1, y1, c) (x2, y2, _) sizex sizey = (x1, y1) : antinodes2' (x1, y1, c) (x2, y2, c) sizex sizey

antinodes2' :: (Int, Int, Char) -> (Int, Int, Char) -> Int -> Int -> [(Int, Int)]
antinodes2' (x1, y1, c) (x2, y2, _) sizex sizey =
    if x1 < 0 || y1 < 0 || x1 >= sizex || y1 >= sizey
    then []
    else (newX1, newY1) : antinodes2' (newX1, newY1, c) (x1, y1, c) sizex sizey
    where diffX = x1 - x2
          diffY = y1 - y2
          newX1 = x1 + diffX
          newY1 = y1 + diffY

part12 :: IO ()
part12 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let (matrix, sizex, sizey) = readMatrix contents
    
    -- for every pair of antennas, find the antinodes
    let antis' = concat $ map (\(a1, a2) -> antinodes a1 a2) [(a1, a2) | a1 <- matrix, a2 <- matrix, a1 /= a2, trd a1 /= '.', trd a2 /= '.', trd a1 == trd a2]
    let antis = nub $ filter (\(x, y) -> x >= 0 && y >= 0 && x < sizex && y < sizey) antis'
    putStrLn $ "Part 1: " ++ show (length antis)

    let antis2' = concat $ map (\(a1, a2) -> antinodes2 a1 a2 sizex sizey) [(a1, a2) | a1 <- matrix, a2 <- matrix, a1 /= a2, trd a1 /= '.', trd a2 /= '.', trd a1 == trd a2]
    let antis2 = nub $ filter (\(x, y) -> x >= 0 && y >= 0 && x < sizex && y < sizey) antis2'
    putStrLn $ "Part 2: " ++ show (length antis2)
