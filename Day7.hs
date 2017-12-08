module Main where

import           Control.Arrow
import           Data.Function   (on)
import           Data.List       (group, maximumBy, sort)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

parseLine line = (head res, (read $ res !! 1)::Int, drop 3 res)
  where res = words $ filter (/= ',') line

part1 prog holdProgs [] = Set.elemAt 0 $ Set.difference prog holdProgs
part1 prog holdProgs ((a,w,holds):xs) = part1 (Set.insert a prog) (Set.union holdProgs $ Set.fromList holds) xs

createMap dict []               = dict
createMap dict ((a,w,holds):xs) = createMap (Map.insert a (w,holds) dict) xs

allEq (x:y:rest) = x==y && allEq (y:rest)
allEq _          = True

count x = length . filter (x==)

findError xs = head $ filter (\x -> count x xs == 1) xs


calc dict cur = if allEq res then w+sum res else error $ show res ++ "," ++ show (erres !! 2) --super dirty and hacky :S
  where
    (w,holds) = dict ! cur
    res = map (calc dict) holds
    erres = map (ercalc dict) holds

ercalc dict cur = res
  where
    (w,holds) = dict ! cur
    res = (cur,w) : map (id &&& calc dict) holds

main = do
  content <- readFile "data/day7.txt"
  let parsedLines =  parseLine <$> lines content
      start =  part1 Set.empty Set.empty parsedLines
  print start
  print $ calc (createMap Map.empty parsedLines) start
