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
import qualified Data.MultiSet   as MSet
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace

infixl 1 £
(£) = ($)

data Instr = SET String String | ADD String String | MUL String String | SUB String String | JNZ String String
    deriving (Read,Show)

parse :: String -> [Instr]
parse =  fmap (read . concat . uncurry (:) . (head &&& (fmap show . tail)) . words) . lines . fmap toUpper


(++++) a b = show a ++ " | " ++ show b

isNumber :: String -> Bool
isNumber str =
    case reads str :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False

findVal :: Map String Int -> String -> Int
findVal dict s = if isNumber s then read s else Map.findWithDefault 0 s dict

(-.) = (-)



--doInstr :: Map String Int -> Instr ->  (Map String Int)
doInstr dict instr =
    case instr of
        SET x y -> Right $ Map.insert x £ findVal dict y £ dict

        MUL x y -> Right $ Map.adjust (* findVal dict y) x dict

        SUB x y -> --(if x == "H" then trace (show (dict ! "H")) else id)
                    Right $ Map.adjust (-. findVal dict y) x dict

        JNZ x y ->  if findVal dict x /= 0 then Left $ findVal dict y else Right dict


isMUL (MUL _ _ ) = trace "HERE" 1
isMUL _          = 0

doAllInstrs dict instrs offset mulCount
    | Just curIns <- Map.lookup offset instrs
    =     case doInstr dict curIns of
            Right d     ->     doAllInstrs d instrs £ offset + 1 £ isMUL curIns + mulCount
            Left offVal ->     doAllInstrs dict instrs £ offset + offVal £ mulCount

    | otherwise = mulCount

doAllInstrs2 prevs dict instrs offset
    | Just curIns <- Map.lookup offset instrs
    =     case
                trace (dict ++++ offset)
                doInstr dict curIns
                of
            Right d     ->     doAll d    instrs £ offset + 1
            Left offVal ->     doAll dict instrs £ offset + offVal
    | otherwise = "hei"
        where doAll = doAllInstrs2 $ Map.insert offset dict prevs




main = do
    content <- readFile "data/day23.txt"
    let instrs = Map.fromList $ zip [0..] $ parse content
        def = Map.fromList $ zip (fmap (:[]) ['A'..'H']) $ repeat 0

    --print $ doAllInstrs Map.empty instrs 0 0
    print $ doAllInstrs def instrs 0 0

    print $ doAllInstrs2 Map.empty (Map.insert "A" 1 def) instrs 0
    return 1
