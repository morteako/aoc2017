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

doInstr :: Map String Int -> Instr -> Either (Either Int Int) (Map String Int)
doInstr dict instr =
    case instr of
        SET x y -> Right (Map.insert x £ findVal dict y £ dict)

        ADD x y -> Right (Map.adjust (+ findVal dict y) x dict)

        MUL x y -> Right (Map.adjust (* findVal dict y) x dict)

        MOD x y -> Right (Map.adjust (`mod` findVal dict y) x dict)

        SND x   -> Right (Map.insert "$SND$" £ findVal dict x £ dict)

        JGZ x y ->  if findVal dict x > 0 then Left $ Right $ findVal dict y  else Right dict

        RCV x   -> if findVal dict x /= 0 then Left $ Left (dict ! "$SND$") else Right dict


doAllInstrs dict instrs offset =
    case res of
        Right d               -> doAllInstrs d instrs $ offset + 1
        Left (Right offs)     -> doAllInstrs dict instrs $ offset + offs
        Left (Left prevSound) -> prevSound

    where
        res = doInstr dict (instrs ! offset)

main = do
    content <- readFile "data/day18.txt"
    let instrs = Map.fromList $ zip [0..] $ parse content
    print instrs
    print $ doAllInstrs Map.empty instrs 0
    return 1
