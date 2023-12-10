module Day10 (day10) where

import Common
import Control.Monad (ap)
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict qualified as M

day10 :: AOCSolution
day10 = map show . ap [part1, part2] . pure . parseInput
        
part1 :: [Point2d] -> Int
part1 = (`div` 2) . length

-- https://en.wikipedia.org/wiki/Pick%27s_theorem
part2 :: [Point2d] -> Int
part2 i = shoelace i + 1 - part1 i

-- https://en.wikipedia.org/wiki/Shoelace_formula
shoelace :: [Point2d] -> Int
shoelace xs = (`div` 2) . abs $ f (last xs : xs)
  where
    f [_] = 0
    f ((x1, y1):p@(x2, y2):xs) = x1 * y2 - x2 * y1 + f (p:xs)

parseInput :: String -> [Point2d]
parseInput input = findLoop $ M.fromList
  [ ((c, r), v)
  | (r, line) <- zip [0..] $ lines input
  , (c, v) <- zip [0..] line,
  v /= '.']

findLoop :: M.HashMap Point2d Char -> [Point2d]
findLoop m = go s start
  where
    (s:_) = M.keys $ M.filter (=='S') m
    (start:_) = neighbours m s
    go :: Point2d -> Point2d -> [Point2d]
    go from p
      | m M.! p /= 'S'
        = p : go p (if a == from then b else a)
      | otherwise
        = [p]
      where
        [a, b] = neighbours m p

neighbours :: M.HashMap Point2d Char -> Point2d -> [Point2d]
neighbours m p@(x, y) = mapMaybe g $ case m M.! p of
  '-' ->
    [ ((x-1, y), "FL-")
    , ((x+1, y), "J7-")
    ]
  '|' ->
    [ ((x, y-1), "F7|")
    , ((x, y+1), "LJ|")
    ]
  'F' ->
    [ ((x+1, y), "J7-")
    , ((x, y+1), "LJ|")
    ]
  '7' ->
    [ ((x-1, y), "FL-")
    , ((x, y+1), "LJ|")
    ]
  'J' ->
    [ ((x-1, y), "FL-")
    , ((x, y-1), "F7|")
    ]
  'L' ->
    [ ((x+1, y), "J7-")
    , ((x, y-1), "F7|")
    ]
  'S' -> 
    [ ((x-1, y), "FL-")
    , ((x+1, y), "J7-")
    , ((x, y-1), "F7|")
    , ((x, y+1), "LJ|")
    ]
  '.' -> []
  where
    f = flip (M.findWithDefault '.') m
    g (p, t) = if f p `elem` 'S':t then Just p else Nothing
