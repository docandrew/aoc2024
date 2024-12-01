-- Build w/ ghc: ghc -o day1 ../AOC.hs day1.hs
import AOC

import System.IO
import Data.List (sort, map)
import Text.Read (readMaybe)

-- read input
main = readLines "input.txt" >>= \lines -> do
  -- parse each line into two words then integers
    let parsedPairs = map parsePair lines
    -- put into parallel lists
    let firsts = sort (map fst parsedPairs)
    let lasts = sort (map snd parsedPairs)
    let sortedPairs = zip firsts lasts

    let dists = map (\(a, b) -> abs (a - b)) sortedPairs
    let sum = foldl (\acc x -> acc + x) 0 dists
    putStrLn ("Part 1: " ++ show sum)

    -- For part 2, iterate through first list and then count occurrences
    -- of each element in the second list, mult by element
    let counts = map (\x -> length (filter (== x) lasts) * x) firsts
    let sum2 = foldl (\acc x -> acc + x) 0 counts
    putStrLn ("Part 2: " ++ show sum2)
    return ()

parsePair :: String -> (Int, Int)
parsePair line = do
    let nums = words line
    (read (head nums), read (last nums))
