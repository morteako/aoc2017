{- Language PatternSynonyms -}

module Main where

import           Control.Arrow
import           Data.Char       (digitToInt, toUpper)
import           Data.Function   (on)
import           Data.List       (foldl', group, maximumBy, sort)
import           Data.Map.Strict (Map, (!))

import qualified Data.Set        as Set

import           Debug.Trace

import           Data.List       (foldl')
import qualified Data.Map.Strict as Map

lim = 130000::Int

createCounts x n dict = if res > lim then dict else createCounts (x+1) n (Map.insertWith (+) res 1 dict)
  where res = sum [x..x+n]

main =
  print $ foldr (+) 0 $ foldl' (flip $ createCounts 1) Map.empty [1..div lim 2]

