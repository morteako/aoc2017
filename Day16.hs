--{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Arrow
import           Data.Char
import           Data.List       (foldl', group, intercalate, sort, unfoldr)
import           Data.List.Split
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Debug.Trace     as Trace

import           Data.Foldable   (toList)
import           Data.Sequence   ((><))
import qualified Data.Sequence   as Seq

data Instr = Spin Int | Exc Int Int | Swap String String deriving Show

doInstr (Spin x)  progs = uncurry (flip (><)) $ Seq.splitAt (Seq.length progs - x) progs
doInstr (Exc a b) progs = Seq.update b prevA seqChangedA
    where
        prevA = Seq.index progs a
        seqChangedA = Seq.update a (Seq.index progs b) progs
doInstr (Swap a b) progs = doInstr (Exc ai bi) progs
    where
        ai  = fromJust $ Seq.elemIndexL a progs
        bi  = fromJust $ Seq.elemIndexL b progs

parse ('s':nr)   = Spin $ read nr
parse ('x':rest) = (\[a,b] -> Exc (read a) (read b)) $ splitOn "/" rest
parse ('p':rest) = (\[a,b] -> Swap a b) $ splitOn "/" rest


doMemo prev count progs instrs
    | Map.member progs prev = fst $ Map.elemAt 0 $ Map.filter (== mod 1000000 count) prev
    | otherwise = doMemo (Map.insert progs count prev) (count+1) (foldl' (flip doInstr) progs instrs) instrs

main = do
    content <- readFile "data/day16.txt"
    let instrs = parse <$> splitOn "," content
        progs = Seq.fromList $ fmap (:[]) ['a'..'p']

    let res = foldl' (flip doInstr) progs instrs
    mapM_ putStr $ toList res

    --part2
    mapM_ putStr $ toList $ doMemo Map.empty (0::Int) progs instrs
