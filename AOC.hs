module AOC (readLines) where

import System.IO

readLines :: String -> IO [String]
readLines filename = do
  contents <- readFile filename
  return (lines contents)
