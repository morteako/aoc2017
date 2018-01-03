{-# LANGUAGE BangPatterns #-}

module Main where



import qualified Data.Map.Strict as Map


import           Data.List.Split (splitOn)




data Layer = PosRange Int Int Int  deriving (Show,Eq,Ord)

parse = Map.fromList . fmap ((\[x,y] -> (x,PosRange 1 y 1)) . fmap read . splitOn ":") . lines

parse2 = fmap ((\[x,y] -> (PosRange x y 1)) . fmap read . splitOn ":") . lines

stepLayer !(PosRange p r curDir)
    | p == 1 && curDir == -1 = PosRange 2 r 1
    | p == r && curDir == 1 = PosRange (p-1) r (-1)
    | otherwise = PosRange (p+curDir) r curDir


step dict pos max
    | pos > max = []
    | Just (PosRange p r _) <- Map.lookup pos dict =
        [pos * r | p == 1] ++ step (Map.delete pos res) (pos+1) max
    | otherwise = step res (pos+1) max
        where
            res = newDict
            newDict = Map.map stepLayer dict

divisible a b = mod a b == 0
willHit delay pos range = divisible (pos+delay) (2*range - 2)

part2 dict delay =
    all f dict
    where
        f (PosRange pos r _) = not $ willHit delay pos r

main = do
    content <- readFile "data/tday13.txt"
    let parsed = parse content
        max = fst $ Map.findMax parsed

    --print $ sum $ step parsed 0 max

    let p2 = parse2 content

        --picosList = take (lim+100) $ iterate (Map.map stepLayer) parsed
        --picosMap = Map.fromList $ zip [0..] picosList
        --f index = step2 picosMap 0 max index
    --print $ Set.size $ Set.fromList picosList
    --print $ head $ filter f [0..lim]
    print $ head $ filter (part2 p2) [0..]
    return 1
