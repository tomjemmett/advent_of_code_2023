module Day11 (day11) where

import Common
import Control.Monad (ap)
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as M
import Data.List (nub, tails, transpose)

type Expansion = Int -> M.HashMap Int Int

day11 :: AOCSolution
day11 = map show . ap (go <$> [1, 999999]) . pure . parseInput

go :: Int -> ([Point2d], Expansion, Expansion) -> Int
go n (i, rows, cols) = sum [manhattanDistance x y | (x : ys) <- tails (nub i'), y <- ys]
  where
    r = rows n
    c = cols n
    i' = map (bimap (c M.!) (r M.!)) i

parseInput :: String -> ([Point2d], Expansion, Expansion)
parseInput input = (i, e ls, e (transpose ls))
  where
    ls = lines input
    e = M.fromList ... expand 0 0
    i =
      [ (c, r)
        | (r, l) <- zip [0 ..] ls,
          (c, v) <- zip [0 ..] l,
          v == '#'
      ]

expand :: Int -> Int -> [String] -> Int -> [(Int, Int)]
expand _ _ [] _ = []
expand a i (x : xs) n = (i, i + a') : expand a' (succ i) xs n
  where
    a' = a + (if all (== '.') x then n else 0)
