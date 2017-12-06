module Main where

import           Data.Function   (on)
import           Data.List       (group, maximumBy, sort)
import qualified Debug.Trace     as Trace

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

createMap nrs = Map.fromList $ zip [0..] nrs

realloc :: Map Int Int -> Map (Map Int Int) Int -> Int -> (Int,Int)
realloc dict prevStates count =
    if Map.member newDict prevStates
    then (count, count - prevStates ! newDict)
    else realloc newDict (Map.insert newDict count prevStates) (count+1)
  where
    (mkey,mval) = maximumBy (compare `on` snd) $ reverse $ Map.assocs dict
    indexes = [mod x (Map.size dict) | x <- [mkey+1..mkey+mval]]
    newDict = foldr f updatedDict indexes
    updatedDict = Map.insert mkey 0 dict
    f i = Map.insertWith (\ _ old -> old + 1) i 0


main = do
  content <- readFile "data/day6.txt"
  let nrs = ((+0) . read) <$> words content
  let (part1,part2) = realloc (createMap nrs) Map.empty 1
  print part1
  print part2
