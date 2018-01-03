module Main where

import           Control.Arrow
import           Data.Bifunctor  as Bi
import           Data.Char       (isDigit, toUpper)
import           Data.Digits
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Monoid
import qualified Data.MultiSet   as MSet
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace

fromList [a,b] = (a,b)

parse = fmap (fromList . sort . fmap (read::String->Int) . splitOn "/") . lines

sumTupList :: [(Int,Int)] -> Int
sumTupList = getSum . foldMap (Sum . uncurry (+))

isIn a (a',b')
    | a == a' = Just (a',b')
    | a == b' = Just (b',a')
    | otherwise = Nothing


findBridges chain@((cur,cur2):_) bridges =
    do
      (a,b) <- bridges
      case isIn cur (a,b) of
        Nothing -> return chain
        Just (a',b') ->
            findBridges ((b',a'):chain) (delete (a,b) bridges)


main = do
    content <- readFile "data/tday24.txt"
    let parsed = parse content
        bs = findBridges [(0,0)] parsed

    print $ maximum $ fmap sumTupList bs

    let withlength = fmap (length &&& id) bs
        maxL = maximum $ fmap fst withlength

    print $ maximum $ (sumTupList . snd) <$> filter ((==maxL) . fst) withlength
