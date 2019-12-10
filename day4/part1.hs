#!/usr/bin/env stack
-- stack --resolver lts-14.16 script --package split

{-# LANGUAGE MonadComprehensions #-}

import Data.List.Split (splitOn)
import Data.Maybe
import Data.List (group, any, foldl')

digits :: Int -> [Int]
digits = fmap (read . return) . show

parse :: String -> (Int, Int)
parse s = (a, b)
  where [a,b] = read <$> splitOn "-" s

testIncreasing :: [Int] -> Bool
testIncreasing = isJust . foldl' f (Just 0)
  where
    f acc cur = [cur | top <- acc, cur >= top]

testLength :: [Int] -> Bool
testLength = (==6) . length

testTwoPlusAdjacent :: [Int] -> Bool
testTwoPlusAdjacent = any (>1) . fmap length . group

check :: Int -> Bool
check = and . sequence tests . digits
  where tests = [testIncreasing, testLength, testTwoPlusAdjacent]

solve :: (Int, Int) -> Int
solve (a,b) = length [ n | n<-[a..b], check n]

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse
