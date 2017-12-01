module Main where

import           Data.Char
import           Test.HUnit

--part1 10 min
--part2 7  min

part1 = do
  str <- readFile "data/day1.txt"
  let full = str ++ [head str]
  let pairs = zip full $ tail full
  print $ sum $ map (digitToInt . fst) $ filter (uncurry (==)) pairs

part2 = do
  str <- readFile "data/day1.txt"
  let l = length str
  let pairs = zip (take (div l 2) str) $ drop (div l 2) str
  print $ sum $ map ((*2) . digitToInt . fst) $ filter (uncurry (==)) pairs

main = do
  part1
  part2
