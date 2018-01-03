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
--import           Data.Text
import           Debug.Trace


type Transition = (String,Int,Int,Int->Int,String)

(#) = flip (.)

min1 x = x - 1

makeTrans :: String -> [String] -> Transition
makeTrans state xs =
    (last $ words state, read ifs ,read write, if move == "left" then min1 else (+1) , cont)
    where
        [ifs,write,move,cont] = res
        res = fmap (last.words) xs


createTrans (state:trans) = [makeTrans state (take 4 trans), makeTrans state (drop 4 trans)]

parse = lines # filter (not.null) # drop 2 # chunksOf 9 # (fmap . fmap . filter) (`notElem` ":.\t-") # filter (not . null) # concatMap createTrans


lim = 12656374

showTrans (s,a,b,_,d) = (s,a,b,d)

step :: Map (String,Int) (Int,Int->Int,String) -> Map Int Int -> Int -> Int -> String -> Int
step prog tape count curPos curS =
    if count == lim then Map.foldr (+) 0 tape
    else
        step prog (Map.insert curPos writeVal tape) (count+1) (tapeFunc curPos) nextState
    where
        (writeVal,tapeFunc,nextState) = prog ! (curS,val)
        val = Map.findWithDefault 0 curPos tape

main = do
    content <- readFile "data/tday25.txt"
    let prog = Map.fromList $ (\(a,b,c,d,e) -> ((a,b),(c,d,e))) <$> parse content
    --print prog
    print $ step prog Map.empty 0 0 "A"
    return 1
