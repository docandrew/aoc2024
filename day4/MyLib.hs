module MyLib (part1, part2) where

import Debug.Trace
import System.IO
import Text.Regex.Posix
import Data.List

part1 :: IO ()
part1 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    
    let matrix = lines contents
    let directions = [N .. NW]

    let width = length (head matrix)
    let height = length matrix

    let valids = map (\direction -> (map (\row -> (map (\col -> (if checkXMAS matrix direction 'X' row col then 1 else 0))) [0..width-1])) [0..height-1]) directions
    let tot = sum $ sumLists $ concat valids
    putStrLn ("part 1:" ++ show tot)

    return ()

part2 :: IO ()
part2 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    
    let matrix = lines contents
    let width = length (head matrix)
    let height = length matrix

    let as = concat $ concat $ map (\dir -> (map (\row -> (map (\col -> check_MAS_2 matrix dir row col)) [0..width-1])) [0..height-1]) [NE, SE, NW, SW]
    let as' = filter (\(a,b) -> a /= -1 && b /= -1) as

    let answer = length $ nub as'
    putStrLn ("part 2:" ++ show answer)
    return ()

sumLists :: (Num a) => [[a]] -> [a]
sumLists = map sum

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Show, Enum)

checkXMAS :: [String] -> Direction -> Char -> Int -> Int -> Bool
checkXMAS mat dir c row col =
    if row < 0 || row >= length mat || col < 0 || col >= length (head mat) -- out of bounds
        then False
    else if mat !! row !! col /= c  -- not the char we're looking for
        then False
    else if c == 'S' -- we made it to XMAS!
        then True
    else checkXMAS mat dir c' row' col'

    where
        (row', col') = case dir of
            N -> (row - 1, col)
            NE -> (row - 1, col + 1)
            E -> (row, col + 1)
            SE -> (row + 1, col + 1)
            S -> (row + 1, col)
            SW -> (row + 1, col - 1)
            W -> (row, col - 1)
            NW -> (row - 1, col - 1)
        c' = case c of
            'X' -> 'M'
            'M' -> 'A'
            'A' -> 'S'
            _   -> '?'  -- will not be used

-- Given a direction and a point, return the opposite 2 points and direction to check for an X
oppositePoints :: Direction -> Int -> Int -> ((Direction, Int, Int), (Direction, Int, Int))
oppositePoints dir row col =
    case dir of
        NE -> ((SE, row - 2, col), (NW, row, col + 2)) -- check point above going SE and point right going NW
        NW -> ((SW, row - 2, col), (NE, row, col - 2)) -- check point above going SW and point left going NE
        SW -> ((NW, row + 2, col), (SE, row, col - 2)) -- check point below going NW and point left going SE
        SE -> ((NE, row + 2, col), (SW, row, col + 2)) -- check point below going NE and point right going SW
        _ -> ((N, -1, -1), (N, -1, -1))

theA :: Direction -> Int -> Int -> (Int, Int)
theA dir row col =
    case dir of
        NE -> (row - 1, col + 1)
        NW -> (row - 1, col - 1)
        SW -> (row + 1, col - 1)
        SE -> (row + 1, col + 1)
        _ -> (-1, -1)

-- Given the matrix, a coordinate and direction, check if MAS is found in any direction
-- and if it is, then check the opposites.
-- we'll return the location of the A, then de-dup and count these at the end.
check_MAS_2 :: [String] -> Direction -> Int -> Int -> (Int, Int)
check_MAS_2 mat dir row col =
    if check_MAS mat dir 'M' row col && (check_MAS mat dir1 'M' row1 col1 || check_MAS mat dir2 'M' row2 col2) then theA dir row col else (-1,-1)
    where 
        ((dir1, row1, col1), (dir2, row2, col2)) = oppositePoints dir row col

-- Every A can only appear once per X.
check_MAS :: [String] -> Direction -> Char -> Int -> Int -> Bool
check_MAS mat dir c row col =
    if row < 0 || row >= length mat || col < 0 || col >= length (head mat) -- out of bounds
        then False
    else if mat !! row !! col /= c  -- not the char we're looking for
        then False
    else if c == 'S' -- we made it to MAS
        then True
    else check_MAS mat dir c' row' col'

    where
        (row', col') = case dir of
            NE -> (row - 1, col + 1)
            SE -> (row + 1, col + 1)
            SW -> (row + 1, col - 1)
            NW -> (row - 1, col - 1)
            _ -> (-1, -1)
        c' = case c of
            'M' -> 'A'
            'A' -> 'S'
            _   -> '?'  -- will not be used