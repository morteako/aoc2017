{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Arrow
import           Data.Bifunctor  as Bi
import           Data.Char       (isDigit, toUpper)
import           Data.Digits
import           Data.Function
import           Data.List       (intercalate, unfoldr)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Debug.Trace

genVal :: Int -> Int -> Int
genVal factor prev = rem (prev*factor) 2147483647


genA = genVal 16807
genB = genVal 48271

as = iterate genA 883
bs = iterate genB 879


main = do
    let get16 = take 16 . digitsRev 2
        f a b = get16 a == get16 b
        divisbleWith a x = mod x a == 0
        as' = filter (divisbleWith 4) as
        bs' = filter (divisbleWith 8) bs

    --part 1
    --print $ length $ filter id $ take 40000000 $ tail $ zipWith f as bs

    --part2
    print $ length $ filter id $ take 5000000 $ tail $ zipWith f as' bs'
    return 1


