#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

calcFuelFor :: Integer -> Integer
calcFuelFor x =
  let fuel = floor (fromIntegral x / 3) - 2
  in if fuel > 0 then fuel + calcFuelFor fuel
                 else 0

solve :: [Integer] -> Integer
solve = sum . fmap calcFuelFor

parse :: String -> [Integer]
parse = fmap read . words

main :: IO ()
main = readFile "input" >>= print . solve . parse
