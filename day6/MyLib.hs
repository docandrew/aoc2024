module MyLib (part12) where

import Debug.Trace
import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
-- import qualified Data.Type.Bool as happens
-- import GHC.Generics (Generic)

part12 :: IO ()
part12 = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let lines = splitOn "\n" contents

    let matrix = concat $ mapIdx (\y line -> mapIdx (\x c -> (x, y, c)) line) lines
    let sizex = length $ head lines
    let sizey = length lines

    -- put matrix in a HashMap for faster lookups
    let matrixMap = HashMap.fromList $ map (\(x, y, c) -> ((x, y), c)) matrix

    let start = fromJust $ find (\(_, _, c) -> c == '^' || c == '>' || c == 'v' || c == '<') matrix
    let startDir = case start of
            (x, y, '^') -> (x, y, N)
            (x, y, '>') -> (x, y, E)
            (x, y, 'v') -> (x, y, S)
            (x, y, '<') -> (x, y, W)
            _ -> error "No start direction found"
    
    let path = walk startDir sizex sizey matrixMap
    let uniques = nub [(x, y) | (x, y, _) <- path]

    putStrLn $ "Part 1: " ++ show (length uniques)

    -- Put our path and matrix in a HashSet for faster loop lookups
    -- remove direction from path and remove duplicates
    let pathCoordsOnly = nub [(x, y) | (x, y, _) <- path]

    -- for each point on our path, try putting a block there and walk to detect a loop.
    -- for every point possible, put a block there and see what happens.
    let loops = map (\tryBlock -> walk2' startDir tryBlock sizex sizey matrixMap) (drop 1 pathCoordsOnly)
    putStrLn $ "Part 2: " ++ show (length $ filter id loops)

    return ()

takeLast :: Int -> [a] -> [a]
takeLast n xs = reverse $ take n $ reverse xs

trd :: (a, b, c) -> c
trd (_, _, c) = c

-- Given an initial direction, a block location, the width and height of the matrix, and the matrix itself,
-- determine if this block location causes a loop in the path.
walk2' :: (Int, Int, Direction) -> (Int, Int) -> Int -> Int -> HashMap.HashMap (Int, Int) Char -> Bool
walk2' (x, y, dir) tryBlock sizex sizey matrix =
    -- trace ("Trying walk starting at " ++ show (x, y, dir) ++ " with # at " ++ show tryBlock ++ " " ++ show res)
    res
    where matrix' = HashMap.insert tryBlock '#' matrix
          res = walk2 (x, y, dir) sizex sizey matrix' HashSet.empty

-- For every point on our path, put a block there, walk it and detect a loop.
-- If we detect a loop, return True, otherwise False.
-- Problem, we are double-counting a block when we rewalk a path from a different direction
walk2 :: (Int, Int, Direction) -> Int -> Int -> HashMap.HashMap (Int, Int) Char -> HashSet.HashSet (Int, Int, Direction) -> Bool
walk2 (x, y, dir) sizex sizey matrix prevSteps
    | x < 0 || y < 0 || x >= sizex || y >= sizey = False
    | isLoop = True
    | otherwise =
    -- trace ("x: " ++ show x ++ ", y: " ++ show y ++ ", dir: " ++ show dir ++ " c: " ++ show c) $
        case c of
            '#' -> walk2 (x, y, lookRight dir) sizex sizey matrix (HashSet.insert (x, y, dir) prevSteps) -- (prevSteps ++ [(x, y, dir), (x, y, lookRight (dir))])
            _ -> walk2 (x', y', dir) sizex sizey matrix (HashSet.insert (x, y, dir) prevSteps) -- (prevSteps ++ [(x, y, dir)])
    where
        (x', y') = case dir of
            N -> (x, y - 1)
            E -> (x + 1, y)
            S -> (x, y + 1)
            W -> (x - 1, y)
        c = charAt (x', y') matrix
        isLoop = HashSet.member (x, y, dir) prevSteps -- (x, y, dir) `elem` prevSteps


charAt :: (Int, Int) -> HashMap.HashMap (Int, Int) Char -> Char
charAt (x, y) matrix = 
    fromMaybe '?' c
    where c = HashMap.lookup (x, y) matrix

-- Given a direction and a position, the width and height of the matrix, iterate through map building a list of 
-- points that we hit. If the next point is a '#', turn right and continue.
walk :: (Int, Int, Direction) -> Int -> Int -> HashMap.HashMap (Int, Int) Char -> [(Int, Int, Direction)]
walk (x, y, dir) sizex sizey matrix = 
    -- trace ("x: " ++ show x ++ ", y: " ++ show y ++ ", dir: " ++ show dir ++ " c: " ++ show c) $
    if x < 0 || y < 0 || x >= sizex || y >= sizey
        then []
        else case c of
            -- at corners record both directions
            '#' -> (x, y, dir) : (x, y, lookRight dir) : walk (x, y, lookRight dir) sizex sizey matrix
            _ -> (x, y, dir) : walk (x', y', dir) sizex sizey matrix
    where
        (x', y') = case dir of
            N -> (x, y - 1)
            E -> (x + 1, y)
            S -> (x, y + 1)
            W -> (x - 1, y)
        c = charAt (x', y') matrix

lookRight :: Direction -> Direction
lookRight N = E
lookRight E = S
lookRight S = W
lookRight W = N

data Direction = N | E | S | W
    deriving (Show, Enum, Eq)

instance Hashable Direction where
    hashWithSalt salt N = hashWithSalt salt (0 :: Int)
    hashWithSalt salt E = hashWithSalt salt (1 :: Int)
    hashWithSalt salt S = hashWithSalt salt (2 :: Int)
    hashWithSalt salt W = hashWithSalt salt (3 :: Int)

mapIdx :: (Int -> a -> b) -> [a] -> [b]
mapIdx f xs = zipWith f [0..] xs
