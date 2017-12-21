module Main where

import           Control.Arrow
import           Data.Function   (on)
import           Data.List       (foldl', maximumBy, partition, sort, sortBy)
import           Data.List.Split (chunksOf, splitOn)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, fromMaybe, isJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace
import           Text.Printf

parseLine :: String -> (Int, [Int])
parseLine str = (read left, fmap read $ splitOn "," $ concat right)
    where
        left:_:right = words str

addPipe :: Map Int (Set Int) -> (Int, [Int]) -> Map Int (Set Int)
addPipe dict (from, tos) = Map.insertWith Set.union from (Set.fromList tos) dict

getGroup :: Map Int (Set Int) -> Int -> Set Int -> Set Int
getGroup dict a curGroup
    | Set.null vals = curGroup
    | otherwise = foldl' (flip (getGroup dict)) (Set.insert a curGroup) $ Set.filter (`Set.notMember` curGroup) vals
        where
            vals = Map.findWithDefault Set.empty a dict

findGroups :: Int -> (Map Int (Set Int), Int) -> (Map Int (Set Int), Int)
findGroups val (dict,count) =
    if Map.size res == 1 then (dict,count) else
        (Map.difference dict res , count+1)
    where
        res = Map.fromList $ zip (Set.toList $ getGroup dict val $ Set.singleton val) [1..]

main :: IO ()
main = do
    content <- readFile "data/day12.txt"

    let parsedLines = parseLine <$> lines content
        pipeMap = foldl' addPipe Map.empty parsedLines
    --part1
    print $ Set.size $ getGroup pipeMap 0 $ Set.singleton 0
    --part2
    let (selfGroups, groupCount) = foldl' (flip findGroups) (pipeMap,0) [1..1999]
    print $ Map.size selfGroups + groupCount


