{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.Arrow
import           Data.Char       (isDigit, toUpper)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Debug.Trace

step = 316


f spin ind 50000000 = Seq.insertAt (1+(ind + step) `mod` (Seq.length spin)) 50000000 spin
f (!spin) (!ind) (!val) =
   -- trace
   -- (show spin ++ "," ++ show ind ++ "," ++ show nextInd)

    f
    (Seq.insertAt (nextInd+1) val spin)
    (nextInd + 1)
    (val+1)
        where
            nextInd = (ind + step) `mod` (Seq.length spin)



part1 = Seq.take 2 $ Seq.dropWhileL (/= 2017) $ f (Seq.singleton 0) 0 1

part2 = Seq.take 2 $ Seq.dropWhileL (/= 0) $ f (Seq.singleton 0) 0 1
--Takes ~1-2 minutes.
--Faster version:


main = do
    print $ part2
