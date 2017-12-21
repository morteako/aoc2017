module Main where

import           Control.Arrow
import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.Function   (on)
import           Data.List       (maximumBy)
import           Data.List.Split (chunksOf, splitOn)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, fromMaybe, isJust)
import qualified Data.Set        as Set
import           Text.Printf

f :: String -> (Double,Double)
f "se" = (1, -0.5)
f "sw" = (-1, -0.5)
f "n"  = (0,1)
f "s"  = (0,-1)
f "nw" = (-1,0.5)
f "ne" = (1,0.5)
f x    = error x

add2 (a,b) (c,d) = (a+c,b+d)

goBack (x,y) count
  | x == 0.0 = count + abs y
  | y > 0 && x > 0 = goBack (x-1,y-0.5) (count+1)
  | y > 0 && x < 0 = goBack (x+1,y-0.5) (count+1)
  | y < 0 && x > 0 = goBack (x-1,y+0.5) (count+1)
  | y < 0 && x < 0 = goBack (x+1,y+0.5) (count+1)
  | x > 0 && y == 0.0 = goBack (x-1,y) (count+1)
  | x < 0 && y == 0.0 = goBack (x+1,y) (count+1)
  | otherwise = error (show (x,y) ++ "," ++ show count)

main = do
  content <- readFile "data/day11.txt"
  let r = f <$> splitOn "," content
      t = scanl1 add2 r

  print $ goBack (foldr1 add2 r) 0
  print $ (`goBack` 0) $ maximumBy (on compare (`goBack` 0)) t

