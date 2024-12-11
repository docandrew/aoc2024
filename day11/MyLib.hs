module MyLib (part12) where

import Debug.Trace
import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe
import Data.Histogram as Histogram

numdigits :: Int -> Int
numdigits x = length $ show x

-- Given a value and a count, return the updated list of values and counts
blinkint :: (Int, Int) -> [(Int, Int)]
blinkint (x, count)
  | x == 0 = [(1, count)]
  | even (numdigits x) = [(fst $ splitint x, count), (snd $ splitint x, count)]
  | otherwise = [(x * 2024, count)]

splitint :: Int -> (Int, Int)
splitint x =
    let s = show x
        n = length s
        (a, b) = splitAt (n `div` 2) s
    in (read a :: Int, read b :: Int)

blink :: Histogram Int -> Histogram Int
blink hist =
    let list = Histogram.toList hist
        list' = concat $ map (\valcount -> blinkint valcount) list
    in Histogram.fromCountList list'

-- Do something a number of times.
redo :: (a -> a) -> Int -> a -> a
redo f 0 a = a
redo f n a = redo f (n - 1) (f a)

part12 :: IO ()
part12 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let list = splitOn " " contents

    -- keep a histogram of everything in each "bucket."
    let hist = Histogram.fromList $ map (\elem -> read elem :: Int) list

    let blinked = redo blink 25 hist
    putStrLn ("part1: " ++ show (size blinked))

    let blinked2 = redo blink 50 blinked
    putStrLn ("part2: " ++ show (size blinked2))
