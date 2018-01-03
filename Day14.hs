module Main where

import           Control.Arrow
import           Data.Bifunctor  as Bi

import           Data.Char       (digitToInt, isAlpha, isDigit, toUpper)
import           Data.Digits
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
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

(#) = flip (.)


toHex = leftPad . digits 2 . digitToInt

convert = digits

leftPad []      = [0,0,0,0]
leftPad [x]     = [0,0,0,x]
leftPad [a,b]   = [0,0,a,b]
leftPad [a,b,c] = [0,a,b,c]
leftPad xs      = xs

createHashes input = fmap ((input++) . ("-"++) . show) [0..127::Int]


lim = 256::Int

dict = Map.fromList $ zip [0..lim-1] [0..lim-1]
f (d,ind,skip) l = (rev d ind l, mod (ind+l+skip) lim, skip+1)

rev dict ind n = foldl f dict (zip inds vals)
  where
    inds = fmap (`mod` lim) [ind..ind+n-1]
    vals = (dict !) <$> reverse inds
    f d (k,v) = Map.insert k v d

makeGrid = fmap (\(y,s) -> zipWith (\x s' -> ((x,y),s')) [0..] s) . zip [0..]


knothash inp =
    let extrals = [17, 31, 73, 47, 23]
        ls = fmap ord inp ++ extrals
        (a,_,_) = foldl f (dict,0,0) $ concat $ replicate 64 ls
        c16 = chunksOf 16 $ snd <$> Map.toAscList a
        xored = fmap (foldr1 xor) c16
    in
    concatMap toHex $ concatMap (printf "%02x" :: Int -> String) xored

toBool 0 = False
toBool 1 = True

dirs = [(1,0),(0,1),(-1,0),(0,-1)]


findRegion :: Set (Int,Int) -> (Int,Int) -> Set (Int,Int) -> (Set (Int,Int),Set (Int,Int))
findRegion grid pos curReg
    |   Set.member pos curReg = (grid,curReg)
    |   Set.member pos grid =
            foldl' helper (Set.delete pos grid,Set.insert pos curReg) $ fmap (addT pos) dirs
    |   otherwise = (grid,curReg)
    where
        addT (a,b) (c,d) = (a+c,b+d)
        helper (g,c) p = findRegion g p c

countRegions :: Set (Int,Int) -> Int
countRegions grid
    | Set.null grid = 0
    | otherwise =
        let
            (newGrid,regPoss) = findRegion grid (Set.elemAt 0 grid) Set.empty
        in
            1 + countRegions (Set.difference newGrid regPoss)



main = do
    print $ sum $ sum . knothash <$> createHashes "oundnydw"
    let grid = Map.keysSet $ Map.filter toBool $ Map.fromList $ concat $ makeGrid $ knothash <$> createHashes "oundnydw"
    print $ countRegions grid

