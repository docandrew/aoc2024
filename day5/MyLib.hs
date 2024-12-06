module MyLib (part12) where

import Debug.Trace
import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe

part12 :: IO ()
part12 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let (rulesStr:updatesStr:_) = splitOn "\n\n" contents
    let rules = [(p1 :: Int, p2 :: Int) | rule <- lines rulesStr, let [p1, p2] = map read (splitOn "|" rule)]

    let updates = map (map read) (map (\line -> (splitOn "," line)) (lines updatesStr)) :: [[Int]]

    let goodUpdates = filter (not . null) $ map (\update -> if checkOrdering rules update then update else []) updates
    let p1 = sumMiddles goodUpdates
    putStrLn ("part 1: " ++ show p1)

    -- filter out the bad updates
    let badUpdates = filter (not . null) $ map (\update -> if checkOrdering rules update then [] else update) updates
    let fixedUpdates = map (\update -> keepFixing rules update) badUpdates

    let p2 = sumMiddles fixedUpdates
    putStrLn ("part 2: " ++ show p2)
    return ()

sumMiddles :: [[Int]] -> Int
sumMiddles us = sum $ map (\u -> u !! ((length u) `div` 2)) us

-- swap elements at index i and j and return the new list
-- this should not be so difficult to do in Haskell...
swap :: [Int] -> Int -> Int -> [Int]
swap [] _ _ = []
swap (x:[]) _ _ = [x]
swap xs i j = let i' = min i j
                  j' = max i j
                  elemI = xs !! i'
                  elemJ = xs !! j'
                  left = take i' xs
                  middle = take (j' - i' - 1) (drop (i' + 1) xs)
                  right = drop (j' + 1) xs
              in left ++ [elemJ] ++ middle ++ [elemI] ++ right

keepFixing :: [(Int, Int)] -> [Int] -> [Int]
keepFixing rules update =
    if checkOrdering rules update then update
    else keepFixing rules (fixOrdering rules update)

fixOrdering :: [(Int, Int)] -> [Int] -> [Int]
fixOrdering [] update = update
fixOrdering (rule:rules) update =
    fixOrdering rules update'
    where idx1 = elemIndex (fst rule) update
          idx2 = elemIndex (snd rule) update
          update' = if (idx1 == Nothing || idx2 == Nothing)
                    then update 
                    else if idx1 < idx2 then
                        update
                    else
                        swap update (fromJust idx1) (fromJust idx2)

-- Given the list of rules and a list of pages, check if the ordering of the pages is correct.
checkOrdering :: [(Int, Int)] -> [Int] -> Bool
checkOrdering [] _ = True
checkOrdering (rule:rules) update =
    res && checkOrdering rules update
    where idx1 = elemIndex (fst rule) update
          idx2 = elemIndex (snd rule) update
          res = if (idx1 == Nothing || idx2 == Nothing) then True else idx1 < idx2
