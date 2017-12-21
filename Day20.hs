module Main where

import           Control.Arrow
import           Data.Bifunctor  as Bi
import           Data.Char       (isDigit, toUpper)
import           Data.Digits
import           Data.Function
import           Data.List       (foldl', intercalate, minimumBy, sortOn,
                                  unfoldr)
import           Data.List.Split
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet   as MSet
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace

type MSet = MSet.MultiSet

type Triple = (Int,Int,Int)
data Info = P Triple | V Triple | A Triple deriving (Show,Eq,Read,Ord)

new '>' = ')'
new '=' = ' '
new '<' = '('
new x   = x

updateVelo (p, V (vx,vy,vz), acc@(A (ax,ay,az))) = (p,V (vx+ax,vy+ay,vz+az) ,acc)

updatePos (P (px,py,pz), vel@(V (vx,vy,vz)), a) = (P (px+vx,py+vy,pz+vz),vel,a)

parse :: String -> [(Info,Info,Info)]
parse = fmap ((\[p,v,a] -> (p,v,a)) . read . (\x -> "[" ++ x ++ "]")) . lines . fmap (toUpper . new)

doUpdate :: [(Int,(Info,Info,Info))] -> [(Int,(Info,Info,Info))]
doUpdate = (fmap . fmap) (updatePos . updateVelo)


manDist (P (x,y,z),_,_) = sum $ fmap abs [x,y,z]


data Particle = Particle Int (Info,Info,Info) deriving (Eq,Show)

instance Ord Particle where
    Particle _ (pos1,_,_) `compare` Particle _ (pos2,_,_) = compare pos1 pos2


parseToSet :: String -> MSet Particle
parseToSet = MSet.fromList . fmap (uncurry Particle) . zip [0..] . parse

doPartUpdate (Particle nr triple) = Particle nr $ updatePos $ updateVelo triple

removeDupls s =
    --trace (show $ MSet.size s) $
    MSet.filter ((<=1) . (`MSet.occur` s)) s

main = do
    content <- readFile "data/day20.txt"
    let particles = zip [0..] $ parse content
        millTimes = foldl' (const . doUpdate) particles [0..10000]

        partSet = parseToSet content
        mtimes = foldl' (const . removeDupls . MSet.map doPartUpdate) partSet [0..100000]
    --print $ minimumBy (on compare (manDist .  snd)) millTimes
    print $ MSet.size mtimes
    return 1


