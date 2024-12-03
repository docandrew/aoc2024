module MyLib (part1, part2) where

import System.IO
import Text.Regex.Posix

part1 :: IO ()
part1 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let pattern = "mul\\([0-9]+,[0-9]+\\)"
    let matches = map head (contents =~ pattern :: [[String]])

    -- For each match, perform the operation
    let result = sum $ map (\(a,b) -> a * b) $ map getNums matches
    putStrLn (show result)
    return ()

part2 :: IO ()
part2 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let pattern = "mul\\([0-9]+,[0-9]+\\)|don't\\(\\)|do\\(\\)"
    let matches = map head (contents =~ pattern :: [[String]])

    let result = sum $ map (\(a,b) -> a * b) $ map getNums $ ffilter True matches
    putStrLn (show result)
    return ()

getNums :: String -> (Int, Int)
getNums s = do
    let a = read (takeWhile (/= ',') (drop 4 s)) :: Int
    let b = read (takeWhile (/= ')') (drop 1 (dropWhile (/=',') s))) :: Int
    (a, b)

-- Filter a list. If we encounter "don't()", skip elements until we get a "do()"
ffilter :: Bool -> [String] -> [String]
ffilter _ [] = []
ffilter True (x:xs) = 
    if x == "don't()" then ffilter False xs 
    else if x == "do()" then ffilter True xs
    else x : ffilter True xs
ffilter False (x:xs) = 
    if x == "do()" then ffilter True xs
    else ffilter False xs
