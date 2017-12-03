module Main where

import           Data.Char
import           Data.List.Split

part1 = do
  str <- readFile "data/day2.txt"
  print $ sum $ ((\ x -> maximum x - minimum x) . fmap read . splitOn "\t") <$> lines str


part2 = do
  str <- readFile "data/day2.txt"
  let nrs = fmap (fmap ((+ 0) . read) . splitOn "\t") $ lines str
  let findDivisible row =
        head [(a,b) | a <- row, b <- row, a /= b, mod a b == 0]
  print $ sum $ fmap (uncurry div . findDivisible) nrs


main = do
  part1
  part2
