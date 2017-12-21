module Main where

import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.List.Split (chunksOf, splitOn)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Text.Printf

lim = 256::Int

dict = Map.fromList $ zip [0..lim-1] [0..lim-1]
f (d,ind,skip) l = (rev d ind l, mod (ind+l+skip) lim, skip+1)

rev dict ind n = foldl f dict (zip inds vals)
  where
    inds = fmap (`mod` lim) [ind..ind+n-1]
    vals = (dict !) <$> reverse inds
    f d (k,v) = Map.insert k v d

part1 = do
  content <- readFile "data/tday10.txt"
  let (a,b,c) = foldl f (dict,0,0) $ read <$> splitOn "," content
  print $ product $ fmap (a !) [0,1]

part2 = do
  content <- readFile "data/tday10.txt"
  let extrals = [17, 31, 73, 47, 23]
      ls = fmap ord content ++ extrals
      (a,b,c) = foldl f (dict,0,0) $ concat $ replicate 64 ls
      c16 = chunksOf 16 $ snd <$> Map.toAscList a
      xored = fmap (foldr1 xor) c16

  mapM_ (putStr . (printf "%02x" :: Int -> String)) xored

main = do
  part1
  part2
