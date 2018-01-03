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
import           Data.Monoid
import qualified Data.MultiSet   as MSet
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace

(#) = flip (.)

data Dir = L | D | R | U deriving (Eq,Enum,Show)



turnLeft U = L
turnLeft d = succ d

turnRight L = U
turnRight d = pred d

f '#' = '.'
f '.' = '#'

move (x,y) d =
    case d of
        L -> (x-1,y)
        R -> (x+1,y)
        U -> (x,y-1)
        D -> (x,y+1)

(++++) a b = show a ++ " : " ++ show b

lim = 10000

step :: Map (Int,Int) Char -> Int -> Int -> (Int,Int) -> Dir -> Int
step dict count acc curPos curDir =
    if count == lim then
        --trace (show dict)
        acc
    else
    step (Map.insert curPos (f res) dict) (count+1) acc' (move curPos newDir) newDir
    where
        res =
            --trace (curPos ++++ curDir ++++ dict) $
            Map.findWithDefault '.' curPos dict
        isInf = res == '#'
        newDir = (if isInf then turnRight else turnLeft) curDir
        acc' = acc + if isInf then 0 else 1



parse = fmap (\(y,s) -> zipWith (\x s' -> ((x,y),s')) [0..] s) . zip [0..] . lines

findMid grid = (id &&& id) $ div (length grid) 2

main = do
    content <- readFile "data/day22.txt"
    let parsed = parse content
        grid = Map.fromList $ concat parsed
    print parsed
    print $ findMid parsed
    print $ step grid 0 0 (findMid parsed) U
    return 1
