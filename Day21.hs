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


data Rule = Rule [String] [String] deriving Show

type RuleMap = Map [String] [String]


a ++++ b = show a ++ " : " ++ show b

(#) = flip (.)

flipPattern :: [[a]] -> [[a]]
flipPattern = fmap reverse

rotateRight :: [[a]] -> [[a]]
rotateRight ([]:_) = []
rotateRight xss    = reverse (fmap head xss) : rotateRight (fmap tail xss)


allPatterns :: String -> [[String]]
allPatterns pat = rots ++ flips
    where
        rots = take 4 . iterate rotateRight . splitOn "/" $ pat
        flips = fmap flipPattern rots

countOn :: [String] -> Int
countOn = length . filter (=='#') . concat

patternSize :: [String] -> Int
patternSize = length

createRuleMap :: [Rule] -> Map [String] [String]
createRuleMap rules = Map.fromList $ fmap (\(Rule pat res ) -> (pat,res)) rules

applyRule :: Map [String] [String] -> [String] -> [String]
applyRule ruleMap partPat =
    ruleMap ! partPat



doAll r2 r3 !pat =
    res
    where
        res = fixGroups newGroups
        rules = if mod patSize 2 == 0 then r2 else r3
        groups = makeGroups groupSize pat
        newGroups = (fmap . fmap) (applyRule rules) groups
        patSize = length pat
        groupSize = if mod patSize 2 == 0 then 2 else 3

makeGroups n xs = chunksOf (div (length $ head xs) n) . concatMap (transpose . fmap (chunksOf n)) . chunksOf n $ xs

fixGroups :: [[[String]]] -> [String]
fixGroups = (concatMap . fmap) concat . fmap transpose


parse :: String -> (RuleMap,RuleMap)
parse =
    (createRuleMap *** createRuleMap) .
    partition (\(Rule ls _) -> length ls == 2) . concatMap (f . splitOn "=>" . filter (not . isSpace) ) .  lines
    where
        f [leftSide,rightSide] =
            (\x -> Rule x (splitOn "/" rightSide)) <$> allPatterns leftSide


main = do
    --print $ allPatterns ".#./..#/###"
    content <- readFile "data/day21.txt"
    --print $ parse content
    let (r2,r3) = parse content
        pat = splitOn "/" ".#./..#/###"
        res1 = (!! 5 ) $ iterate (doAll r2 r3) pat
        res2 = (!! 18) $ iterate (doAll r2 r3) pat
    print $ countOn res1
    print $ countOn res2


