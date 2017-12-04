module Main where

import           Data.List       (sort)


noDupls xs = length xs == Set.size (Set.fromList xs)

noAnas :: [String] -> Bool
noAnas = noDupls . fmap sort

part1 = do
  input <- readFile "data/day4.txt"
  let xs = fmap words $ lines input
  print $ length $ filter noDupls xs
  print $ length $ filter noAnas xs
main = part1
