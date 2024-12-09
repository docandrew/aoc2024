{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module MyLib (part12) where

import Debug.Trace
import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable

-- Parse line into the answer and the list of numbers in the problem
parseProblem :: String -> (Int, [Int])
parseProblem line = (read answer, map read numbers)
    -- trace ("parts: " ++ show parts) $
    where   parts = splitOn ": " line
            answer = head parts
            numbers = splitOn " " (last parts)

showSolution :: (Int, [Int]) -> [Int -> Int -> Int] -> String
showSolution (answer, numbers) [] = "No solutions"
showSolution (answer, numbers) ops = show answer ++ " = " ++ showSolution' numbers ops

showSolution' :: [Int] -> [Int -> Int -> Int] -> String
showSolution' [] _ = ""
showSolution' [num] [] = show num
showSolution' (num:nums) (op:ops) =
    case op of
        (*) -> show num ++ " * " ++ showSolution' nums ops
        (+) -> show num ++ " + " ++ showSolution' nums ops
        cat -> show num ++ " || " ++ showSolution' nums ops
        _ -> "Unknown operation"

-- Concatenate two numbers together
cat :: Int -> Int -> Int
cat x y = read (show x ++ show y) :: Int

catLast2 :: [Int] -> [Int]
catLast2 xs = case reverse xs of
    y:x:rest -> reverse (cat x y : rest)
    _ -> xs

-- Given the answer, and a list of numbers, we usually solve
-- from left-to-right. In this case, we'll go from right to left
-- and build up a tree of possible solutions. If at any point we
-- can't make it work, we can backtrack in the other branches.
-- i.e. 100: 5 5 4.
-- Start with subtraction. 100 - 4 = 96. Then we try to divide 96 by 5. Not an integer, it doesn't work. (try modulo) Stop here.
-- Then we try to subtract 5 from 96 = 91 since this doesn't equal the first number, it doesn't work.
-- Back at top-level, try 100 / 4 = 25. Then try 25 / 5 = the first number. Success.
solve :: (Int, [Int]) -> [Int -> Int -> Int]
solve (answer, numbers) = reverse $ solve' answer answer numbers

-- Given the answer, an accumulator, a list of numbers, return a list of / or - operations to go from the accumulator to the answer.
solve' :: Int -> Int -> [Int] -> [Int -> Int -> Int]
solve' answer acc numbers =
    -- trace ("answer: " ++ show answer ++ ", numbers: " ++ show numbers) $
    if length numbers == 2
        then
            if acc == top * head numbers                -- avoid integer division issues here.
                then [(*)]
            else ([(+) | acc - top == head numbers])    -- If acc - top is the last number, then subtraction works.
        else
            -- Try to rule out division or subtraction here to prevent steps.
            if not (null solveDiv) then (*) : solveDiv
            else if not (null solveSub) then (+) : solveSub
            else []
    where top = last numbers
          solveDiv = if acc `mod` top == 0 then solve' answer (acc `div` top) (take (length numbers - 1) numbers) else []
          solveSub = if acc - top >= head numbers then solve' answer (acc - top) (take (length numbers - 1) numbers) else []

-- Like solve, but if the other two operations don't work, attempt to concatenate them.
solve2 :: (Int, [Int]) -> [Int -> Int -> Int]
solve2 (answer, numbers) = solve2' True answer 0 numbers

solve2' :: Bool -> Int -> Int -> [Int] -> [Int -> Int -> Int]
solve2' initacc answer acc [x] =
    if cat acc x == answer then [cat]
    else if acc * x == answer then [(*)]
    else if acc + x == answer then [(+)]
    else []
solve2' initacc answer acc (x:xs) =
    -- trace ("answer: " ++ show answer ++ ", numbers: " ++ show (x:xs)  ++ " acc:" ++ show acc) $
    if not (null (solve2' False answer catacc xs)) then cat : solve2' False answer catacc xs
    else if not (null (solve2' False answer addacc xs)) then (+) : solve2' False answer addacc xs
    else if not (null (solve2' False answer mulacc xs)) then (*) : solve2' False answer mulacc xs
    else []
    where 
        catacc = if initacc then x else cat acc x
        addacc = if initacc then x else acc + x
        mulacc = if initacc then x else acc * x

part12 :: IO ()
part12 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let lines = splitOn "\n" contents

    let problems = map parseProblem lines
    putStrLn $ show problems

    let solutions = map solve problems
    let probSols = zip problems solutions
    -- putStrLn $ show probSols
    let solStrs = map (\(p, s) -> showSolution p s) probSols

    mapM_ putStrLn solStrs

    -- sum all the probSols with non-empty solutions
    let sum = foldl (\acc (p, s) -> if null s then acc else acc + fst p) 0 probSols
    putStrLn $ "Part 1: " ++ show sum

    -- run it again with solve2
    let solutions2 = map solve2 problems
    let probSols2 = zip problems solutions2
    let solStrs2 = map (\(p, s) -> showSolution p s) probSols2
    mapM_ putStrLn solStrs2

    let sum2 = foldl (\acc (p, s) -> if null s then acc else acc + fst p) 0 probSols2
    putStrLn $ "Part 2: " ++ show sum2
