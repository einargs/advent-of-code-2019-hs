#!/usr/bin/env stack
-- stack --resolver lts-14.16 script --package split --package containers

import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap, (!))
import Debug.Trace
import Data.List.Split (splitOn)

type Program = IntMap Int
type Position = Int

deref :: Program -> Position -> Int
deref pgrm pos = pgrm ! (pgrm ! pos)

step :: Position -> Program -> Maybe Program
step pos pgrm = case opcode of
    1 -> Just $ perform (+)
    2 -> Just $ perform (*)
    99 -> Nothing
    x -> error $ "Impossible opcode " ++ show x ++ " at pos " ++ show pos
  where
    opcode = pgrm ! pos
    arg1 = pgrm `deref` (pos+1)
    arg2 = pgrm `deref` (pos+2)
    loc = pgrm ! (pos+3)
    perform :: (Int -> Int -> Int) -> Program
    perform op = IM.insert loc (arg1 `op` arg2) pgrm

run :: Position -> Program -> Program
run pos pgrm =
  case step pos pgrm of
    Nothing -> pgrm
    Just pgrm -> run (pos+4) pgrm

parse :: String -> Program
parse = buildMap . fmap read . splitOn ","
  where
    buildMap :: [Int] -> Program
    buildMap = IM.fromList . zip [0..]

provideArgs :: Int -> Int -> Program -> Program
provideArgs a1 a2 = IM.insert 1 a1 . IM.insert 2 a2

runWithArgs :: Int -> Int -> Program -> Int
runWithArgs a1 a2 = (!0) . run 0 . provideArgs a1 a2

solve :: Program -> (Int, Int)
solve pgrm = 
  head [(x,y) | x <- range,
                y <- range, 
                runWithArgs x y pgrm == 19690720 ]
  where range = [0..99]

inSolutionForm :: (Int, Int) -> Int
inSolutionForm (noun, verb) = noun * 100 + verb

main :: IO ()
main = readFile "input.txt" >>= print . inSolutionForm . solve . parse

