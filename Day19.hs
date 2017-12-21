{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.Arrow
import           Data.Char       (isDigit, toUpper)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Debug.Trace


data GridMark = Line | Cross | Blank | Letter Char deriving (Eq,Ord,Show)
data Direction = Up | L | Down | R deriving (Enum, Ord, Eq,Show)

readGrid '|'    = Line
readGrid '-'    = Line
readGrid '+'    = Cross
readGrid ' '    = Blank
readGrid letter = Letter letter

parse =
    Map.fromList . concatMap (\(ind, grids) -> fmap (first ((,) ind)) grids) . zip [0..] .
        fmap (zip [0..] . fmap readGrid) . lines

getNext (y,x) dir =
    case dir of
        Up   -> (y-1,x)
        Down -> (y+1,x)
        L    -> (y,x-1)
        R    -> (y,x+1)

mySucc R = Up
mySucc x = succ x

myPred Up = R
myPred x  = pred x

move :: Map (Int,Int) GridMark -> Int -> (Int,Int) -> Direction -> String -> (String, Int)
move grid count pos dir letters =
    case curGrid of
        Letter l -> moveG (getNext pos dir) dir $ l:letters
        Line     -> moveG (getNext pos dir) dir letters
        Cross    -> moveG (getNext pos crossDir) crossDir letters
        Blank    -> (reverse letters, count)
    where
        curGrid = grid ! pos
        moveG = move grid (count + 1)
        checkSides newDir =  Blank /= (grid ! getNext pos newDir)
        crossDir = head $ filter checkSides [mySucc dir, myPred dir]

startPoint grid = head $ filter (\pos -> Blank /= grid ! pos) $ fmap ((,) 0) [0..]

main = do
    content <- readFile "data/day19.txt"
    print content
    let parsed      = parse content
        startPos    = startPoint parsed

    print $ move parsed 0 startPos Down ""


