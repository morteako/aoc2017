module Main where

import           Control.Arrow
import           Data.Char
import           Data.List.Split
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Debug.Trace     as Trace


--Horrible part 2 code :O

--part1
--data Dir = L | U | R | D deriving (Enum, Show,Eq)

data Dir =  U | L | D | R deriving (Enum, Show,Eq)


findPos (x,y) nr target n dirCount dir
  | nr == target = (x,y)
  | dirCount == n =

      findPos (x,y)             nr      target n 0           (succ dir)
  | otherwise =
      findPos (next (x,y) dir)  (nr-1)  target n (dirCount+1) dir

next (x,y) dir =
  eTrace "POS:" (case dir of
    L -> (x-1,y)
    R -> (x+1,y)
    D -> (x,y-1)
    U -> (x,y+1))


part1 = do
  let input = 289326
  let f = (^2) . (+1) . (*2)
  let (actual,closest) = head $ dropWhile ((<input) . snd) $ map (id &&& f) [0..]
  let n = fromIntegral $ round $ sqrt $ fromInteger closest
  let res = findPos (actual,negate actual) closest input (n-1) (0::Integer) L

  print $ abs $ uncurry (+) $ (abs *** abs) res

debug = False

trace str x = if debug then Trace.trace str x else x

baseTrace x = trace (show x) x

eTrace s x = trace (s ++ show x) x



--trav :: (Int,Int) -> Int -> Int -> Int -> Dir -> Map.Map (Int,Int) Int -> Int
trav target (x,y) n dir dict
  -- | x > 20 = error "DONE"
  | x == n && dir == R =
    get $ trav target (next (x,y) R) (n+1) U newDict
  | x == negate n && dir == L =
    get $ trav target (next (x,y) D) n D newDict
  | y == n && dir == U =
      get $ trav target (next (x,y) L) n L newDict
  | y == negate n && dir == D =
    get $ trav target (next (x,y) R) n R newDict
  | otherwise =
        get $ trav target (next (x,y) dir)   n dir    newDict
    where
      combs = map (\[x,y] -> (x,y)) $ mapM (const [-1,0,1]) [1,2]
      neighs = baseTrace $ map (addTuple (x,y)) combs
      lookVals = baseTrace $ mapMaybe (`Map.lookup` dict) neighs
      val = trace (show lookVals ++ show dir ++ "," ++ show n ) sum lookVals
      newDict = Map.insert (x,y) val dict
      retVal = (val,newDict)

      get x = if val > target then retVal else x

addTuple (a,b) (c,d) = (a+c,b+d)

part2 = do
  let input = 289326

  print $ trav 289326 (0,0) 0 R $ Map.singleton (0,0) 1


main = part2
