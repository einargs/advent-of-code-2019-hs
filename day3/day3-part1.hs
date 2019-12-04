#!/usr/bin/env stack
-- stack --resolver lts-14.16 script --package split

{-# LANGUAGE LambdaCase #-}

import Numeric.Natural
import Data.Maybe (maybeToList)
import Debug.Trace

import Data.List.Split (splitOn)

debug :: Show a => String -> a -> a
debug str x = trace (str ++ " " ++ show x) x

data Turn = TurnRight Int
          | TurnLeft Int
          | TurnUp Int
          | TurnDown Int
          deriving Show

type Path = [Turn]

data Paths = Paths { first :: Path, second :: Path }
  deriving Show

data Point = Point { x::Int, y::Int }
  deriving (Show, Eq)

origin :: Point
origin = Point 0 0

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) =
  abs (x1 - x2) + abs (y1 - y2)

data Line = Line Point Turn
  deriving Show

addTurnToPoint :: Point -> Turn -> Point
addTurnToPoint (Point x y) = \case
  TurnRight i -> Point (x+i) y
  TurnLeft i -> Point (x-i) y
  TurnUp i -> Point x (y+i)
  TurnDown i -> Point x (y-i)

endpoints :: Line -> (Point, Point)
endpoints (Line p1 t) = (p1, addTurnToPoint p1 t)

pathToLines :: Point -> Path -> [Line]
pathToLines p [] = []
pathToLines p (t:ts) = line:pathToLines p' ts
  where
    p' = addTurnToPoint p t
    line = Line p t

parseTurn :: String -> Turn
parseTurn (dirChar:numStr) = parseDir dirChar $ read numStr
  where
    parseDir :: Char -> Int -> Turn
    parseDir 'U' = TurnUp
    parseDir 'L' = TurnLeft
    parseDir 'R' = TurnRight
    parseDir 'D' = TurnDown
    parseDir _ = error "Should be impossible"

parsePath :: String -> Path
parsePath = fmap parseTurn . splitOn ","

parsePaths :: String -> Paths
parsePaths str =
  let [f,s] = parsePath <$> lines str
  in Paths f s

intersection :: Line -> Line -> Maybe Point
intersection l0 l1 =
  if isIntersection
     then Just $ Point ix iy
     else Nothing
  where
    (p0, p1) = endpoints l0
    (p2, p3) = endpoints l1
    s1x = x p1 - x p0
    s1y = y p1 - y p0
    s2x = x p3 - x p2
    s2y = y p3 - y p2
    s, t, thing :: Double
    thing = fromIntegral $ -s2x * s1y + s1x * s2y
    s = fromIntegral (-s1y * (x p0 - x p2) + s1x * (y p0 - y p2))  / thing
    t = fromIntegral ( s2x * (y p0 - y p2) - s2y * (x p0 - x p2)) / thing
    ix, iy :: Int
    ix = floor $ fromIntegral (x p0) + (t * fromIntegral s1x)
    iy = floor $ fromIntegral (y p0) + (t * fromIntegral s1y)
    isIntersection = s >= 0 && s <= 1 && t >= 0 && t <= 1

intersectionsOf :: [Line] -> [Line] -> [Point]
intersectionsOf lines1 lines2 = do
  l1 <- lines1
  l2 <- lines2
  maybeToList $ intersection l1 l2

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (Paths first second) = parsePaths input
      toLines = pathToLines origin
      first' = toLines first
      second' = toLines second
      intersections = intersectionsOf first' second'
      answer = minimum $ manhattanDistance origin <$> intersections
  print answer
  return ()

