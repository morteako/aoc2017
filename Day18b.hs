module Main (main) where

import           Control.Arrow
import           Data.Char       (isDigit, toUpper)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Debug.Trace

infixl 0 £
(£) = ($)

data Instr = SET String String | ADD String String | MUL String String | MOD String String | SND String | RCV String | JGZ String String
    deriving (Read,Show)

data Program =
    Program
    {progId :: Int, msgQ :: [Integer] , sndCount :: Integer, regs :: Map String Integer , offset :: Integer, waiting :: Bool}
    deriving Show


parse :: String -> [Instr]
parse =  fmap (read . concat . uncurry (:) . (head &&& (fmap show . tail)) . words) . lines . fmap toUpper


--Debug
(++++) a b = show a ++ " |\n" ++ show b ++ "\n"

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

findVal :: Map String Integer -> String -> Integer
findVal dict s = if isNumber s then read s else Map.findWithDefault 0 s dict

doPara :: Program -> Program -> Map Integer Instr -> (Program, Program)
doPara curProg otherProg instrs
    | waiting curProg && waiting otherProg = (curProg, otherProg)
    | waiting curProg = doPara otherProg curProg instrs
    | otherwise =
        case instr of
            SET x y ->
                doPara
                (f (Map.insert x £ findVal dict y £ dict) 1)
                otherProg
                instrs

            ADD x y ->
                doPara
                (f (Map.adjust (+ (findVal dict y)) x dict) 1)
                otherProg
                instrs

            MUL x y ->
                doPara
                (f (Map.adjust (* findVal dict y) x dict) 1)
                otherProg
                instrs

            MOD x y ->
                doPara
                (f (Map.adjust (`mod` findVal dict y) x dict) 1)
                otherProg
                instrs

            JGZ x y ->
                doPara
                (f dict (if findVal dict x > 0 then findVal dict y else 1))
                otherProg
                instrs

            SND x   ->
                doPara
                (curProg {offset = offset curProg + 1, sndCount = sndCount curProg + 1})
                (otherProg {waiting = False, msgQ = msgQ otherProg ++ [findVal dict x]})
                instrs

            RCV x   ->
                doPara
                (case msgQ curProg of
                    [] ->  curProg {waiting = True}
                    (v:vs) -> curProg {regs = (Map.insert x v dict), offset = offset curProg + 1, msgQ = vs})
                otherProg
                instrs

    where
        f r o = curProg {regs = r, offset = offset curProg + o}
        dict = regs curProg
        instr = (instrs ! offset curProg)



main = do
    content <- readFile "data/day18.txt"
    let instrs = Map.fromList $ zip [0..] $ parse content
    let prog0 = Program {progId = 0, msgQ = [], regs = Map.singleton "P" 0, offset = 0,waiting = False,sndCount = 0}
    let prog1 = Program {progId = 1, msgQ = [], regs = Map.singleton "P" 1, offset = 0,waiting = False,sndCount = 0}
    print $ doPara prog0 prog1 instrs
    return 1
