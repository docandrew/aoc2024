module MyLib (part12) where

import Debug.Trace
import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import qualified Data.Vector as V

-- Given a function, map that function over a list, but also pass the index of the element to the function.
mapIdx :: (Int -> a -> b) -> [a] -> [b]
mapIdx f = zipWith f [0..]

trd :: (a, b, c) -> c
trd (_, _, c) = c

readChar :: Read a => Char -> a
readChar c = read [c]

toIntArray :: String -> [Int]
toIntArray = map (read . return) . filter (/= '\n')

-- Given the list of integers from the disk layout
processList :: [Int] -> [Maybe Int]
processList disk = processList' disk 0 False

-- If free then this next number is the length of free space
processList' :: [Int] -> Int -> Bool -> [Maybe Int]
processList' [] _ _ = []
processList' (x:xs) id free =
    if free then replicate x Nothing ++ processList' xs id False
            else replicate x (Just id) ++ processList' xs (id + 1) True

-- iterate through blocks, every time we encounter "Nothing", take from the front of the reversed list
compact :: [Maybe Int] -> [Int] -> [Int]
compact [] _ = []
compact (x:xs) (y:ys) = 
    case x of
        Just a -> a : compact xs (y:ys)
        Nothing -> y : compact xs ys

-- Empty with length of empty space, or Used with length of used space and the file number
data FSBlock = Empty Int | Used Int Int deriving (Eq, Show)

-- Converts a list of Maybe Int to a list of FSBlock, where consecutive Nothings are grouped into Empty blocks
-- and consecutive Just values are grouped into Used blocks with the corresponding file number.
toBlocks :: [Maybe Int] -> [FSBlock]
toBlocks [] = []
toBlocks (x:xs) = 
    case x of
        Just a -> let (used, rest) = span (== Just a) xs
                   in Used (1 + length used) a : toBlocks rest
        Nothing -> let (empty, rest) = span (== Nothing) xs
                   in Empty (1 + length empty) : toBlocks rest

compact2 :: [FSBlock] -> [FSBlock] -> [FSBlock]
compact2 [] back = back
compact2 []  _ = []
compact2 [x] back = x : back
-- blocks, back
compact2 list back =
    case end of
        Empty numEmpty ->
            compact2 (init list) (end:back)
        Used needed val ->
            if not (null (search end list)) then compact2 (init (search end list)) (Empty needed : back)
            else compact2 (init list) (end:back)
    where end = last list

-- Given a block, search for a home for it and return the updated list
search :: FSBlock -> [FSBlock] -> [FSBlock]
search _ [] = []
search needle (x:xs) =
    case (x, needle) of
        (Empty numEmpty, Used needed val) ->
            if numEmpty > needed then needle : Empty (numEmpty - needed) : xs
            else if numEmpty == needed then needle : xs
            else if not (null (search needle xs)) then x : search needle xs
            else []
        (Used _ _, _) -> 
            if not (null (search needle xs)) then x : search needle xs
            else []

-- Given a list of FSBlock, replicate the Ints and discard the empties.
expand :: [FSBlock] -> [Int]
expand [] = []
expand (l:ls) =
    case l of
        Empty numEmpty -> replicate numEmpty 0 ++ expand ls
        Used needed val -> replicate needed val ++ expand ls

part12 :: IO ()
part12 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let disk = map (readChar :: Char -> Int) contents

    let blocks = processList disk
    let rev = map fromJust $ reverse (filter (/= Nothing) blocks)
    let compressed = take (length rev) $ compact blocks rev
    let p1 = sum $ zipWith (*) [0..] compressed
    putStrLn $ "Part 1: " ++ show p1

    let blocks' = toBlocks blocks
    let compressed2' = expand $ compact2 blocks' []
    let p2 = sum $ zipWith (*) [0..] compressed2'
    putStrLn $ "Part 2: " ++ show p2
