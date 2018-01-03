{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Arrow
--import           Data.Bifunctor  as Bi

import           Data.Char       (digitToInt, isAlpha, isDigit, isSpace,
                                  toUpper)
import           Data.Digits
import           Data.Either
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, fromJust, mapMaybe)
import           Data.Monoid
import qualified Data.MultiSet   as MSet
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace
import           Text.Printf

import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.List.Split (chunksOf, splitOn)


data State = Garbage | Cancel | Normal

step :: Int -> State -> String -> Int
step depth _ []             = 0
step depth Cancel (_:xs)    = step depth Garbage xs
step depth Garbage ('>':xs) = step depth Normal xs
step depth Garbage ('!':xs) = step depth Cancel xs
step depth Garbage (x:xs)   = step depth Garbage xs
step depth Normal ('{':xs)  = step (depth+1) Normal xs
step depth Normal ('}':xs)  = depth + step (depth-1) Normal xs
step depth Normal ('<':xs)  = step depth Garbage xs
step depth Normal (_:xs)    = step depth Normal xs

part2 = step 0 Normal
    where
        step :: Int -> State -> String -> Int
        step depth _ []             = 0
        step depth Cancel (_:xs)    = step depth Garbage xs
        step depth Garbage ('>':xs) = step depth Normal xs
        step depth Garbage ('!':xs) = step depth Cancel xs
        step depth Garbage (x:xs)   = 1+step depth Garbage xs
        step depth Normal ('{':xs)  = step (depth+1) Normal xs
        step depth Normal ('}':xs)  = step (depth-1) Normal xs
        step depth Normal ('<':xs)  = step depth Garbage xs
        step depth Normal (_:xs)    = step depth Normal xs

start str = step 0 Normal str

main = do
    content <- readFile "data/day9.txt"
    print $ start content
    print $ part2 content
