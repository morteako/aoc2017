module Main where

import           Control.Arrow
import           Data.Bifunctor  as Bi
import           Data.Char       (isAlpha, isDigit, toUpper)
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
(#) = flip (.)

data Dir = L | D | R | U deriving (Eq,Enum,Show)

data Status = Clean | Weakened | Infected | Flagged deriving (Eq,Enum,Show)

next Flagged = Clean
next x       = succ x

turnLeft U = L
turnLeft d = succ d

turnRight L = U
turnRight d = pred d


move (x,y) d =
    case d of
        L -> (x-1,y)
        R -> (x+1,y)
        U -> (x,y-1)
        D -> (x,y+1)

(++++) a b = show a ++ " : " ++ show b

lim = 10000000

step :: Map (Int,Int) Status -> Int -> Int -> (Int,Int) -> Dir -> Int
step dict count acc curPos curDir =
    if count == lim then
        acc
    else
    step (Map.insert curPos (next res) dict) (count+1) acc' (move curPos nextDir) nextDir
    where
        res = Map.findWithDefault Clean curPos dict
        nextDir = newDir res curDir
        acc' = acc + if res == Weakened then 1 else 0

newDir Clean x    = turnLeft x
newDir Weakened x = x
newDir Infected x = turnRight x
newDir Flagged x  = turnRight $ turnRight x

parse = fmap (\(y,s) -> zipWith (\x s' -> ((x,y),s')) [0..] s) . zip [0..] . (fmap . fmap) f . lines
    where
        f :: Char -> Status
        f = toEnum . fromJust . (`elemIndex` ".W#F")

findMid grid = (id &&& id) $ div (length grid) 2

main = do
    content <- readFile "data/day22.txt"
    let parsed = parse content
        grid = Map.fromList $ concat parsed
    print parsed
    print $ findMid parsed
    print $ step grid 0 0 (findMid parsed) U
    return 1
