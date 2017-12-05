module Main where

import           Control.Arrow
import           Data.Char
import           Data.List       (group, sort)
import           Data.List.Split
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Debug.Trace     as Trace


createMap nrs = Map.fromList $ zip [0..] nrs

doInstrs :: Map.Map Int Int -> Int -> Int -> Int
doInstrs dict count cur =
  if Map.notMember cur dict then count
  else
    doInstrs newDict (count+1) next
  where
    next = cur + (dict ! cur)
    newDict = Map.insertWith f cur 0 dict
    f _ a = if a >= 3 then a-1 else a+1
    --f _ a = a +1          --PART 1

main = do
  content <- readFile "data/day5.txt"
  print $ doInstrs (createMap $ read <$> lines content) 0 0
