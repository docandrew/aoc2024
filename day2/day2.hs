-- Build w/ ghc: ghc -o day2 ../AOC.hs day2.hs
import AOC

import System.IO
import Data.Foldable (forM_)
import Data.List (sort, map, sum)
import Text.Read (readMaybe)

-- read input
main = readLines "input.txt" >>= \lines -> do
    let reports = map (\x -> map (\y -> read y :: Int) x) (map words lines)

    let numSafe = sum (map (\x -> if isSafe x then 1 else 0) reports)
    putStrLn ("Part 1: " ++ show numSafe)
    
    let numSafe2 = sum (map (\report -> if isSafe report then 1 else (if True `elem` (map (\y -> isSafe y) (oneOut report)) then 1 else 0)) reports)
    putStrLn ("Part 2: " ++ show numSafe2)
    return ()

-- Given a list, calculate distance between each element and its neighbor
dist :: (Integral a) => [a] -> [a]
dist [] = []
dist [x] = []
dist (x:y:xs) = y - x : dist (y:xs)

-- Given a list, determine if its all negative or positive
allSameSign :: (Integral a) => [a] -> Bool
allSameSign [] = True
allSameSign [x] = True
allSameSign (x:y:xs) = (x > 0 && y > 0 || x < 0 && y < 0) && allSameSign (y:xs)

-- Given a list, determine if absolute value of any element is greater than 3
gt3 :: (Integral a) => [a] -> Bool
gt3 [] = False
gt3 (x:xs) = abs x > 3 || gt3 xs

-- Given a list of dists, determine if it is safe
isSafe :: (Integral a) => [a] -> Bool
isSafe report = allSameSign (dist report) && not (gt3 (dist report))

-- Return a list of all possible lists with one element removed
oneOut' :: (Integral a) => [a] -> [[a]]
oneOut' [] = []
oneOut' [x] = []
oneOut' (x:xs) = xs : map (x:) (oneOut' xs)

oneOut :: (Integral a) => [a] -> [[a]]
oneOut [] = []
oneOut [x] = []
oneOut (x:xs) = oneOut' (x:xs) ++ [init (x:xs)]
