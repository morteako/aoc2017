module Main where

--import           Control.Arrow
import           Data.Char       (toUpper)
import           Data.Function   (on)
import           Data.List       (group, maximumBy, sort)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

--import           Control.Monad.State

--import           Debug.Trace

data Instr = Instr String Modifier String String Int deriving Show


data Modifier = INC Int | DEC Int deriving (Read, Show)

doMod (INC i) new old = old + i
doMod (DEC i) new old = old - i

funcs :: Map String (Int -> Int -> Bool)
funcs = Map.fromList [(">",(>)), (">=",(>=)), ("<",(<)), ("<=",(<=)), ("==",(==)),("!=",(/=))]

parseLine :: [Char] -> Instr
parseLine s = Instr target (read $ modifier ++ " " ++ nr) left func (read right)
  where
     [target, modifier, nr, _, left, func, right] = words $ fmap toUpper s

trace s a = a

doInstr :: Instr -> Map String Int -> Map String Int
doInstr instr@(Instr target modifier left funcKey right) dict =
  if trace (show instr) (funcs ! funcKey) (Map.findWithDefault 0 left dict) right
    then  f $ Map.insertWith (doMod modifier) target from0 dict
    else trace (show dict) dict

  where from0 = (doMod modifier 0 0)
        f d = Map.insertWith max "$MAX$" (Map.findWithDefault from0 target d) d
main = do
  content <- readFile "data/day8.txt"
  let instrs =  fmap parseLine $ lines content
  print instrs
  let res = foldl (flip doInstr) Map.empty instrs
  print res
  print $ maximum $ fmap snd $ Map.toList res
  print $ res ! "$MAX$"
