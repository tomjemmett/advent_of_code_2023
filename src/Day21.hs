module Day21 where -- (day21) where

import Common
import Control.Monad (ap)
import Control.Monad.Writer
import Data.HashSet qualified as S
import Data.List (find)
import Text.Parsec qualified as P

day21 :: AOCSolution
day21 = map show . ap [part1, part2] . pure . parseInput

go :: Int -> (S.HashSet Point2d, Point2d) -> Int
go i (g, s) = S.size $ step g i $ S.singleton s

part1 :: (S.HashSet Point2d, Point2d) -> Int
part1 = go 64

part2 :: (S.HashSet Point2d, Point2d) -> Int
part2 i = b0 + b1 * goal + (n * pred n `div` 2) * (b2 - b1)
  where
    goal = 26501365
    n = goal `div` 131
    [a0, a1, a2] = map (\x -> go (65 + x * 131) i) [0 .. 2]
    b0 = a0
    b1 = a1 - a0
    b2 = a2 - a1

step :: S.HashSet Point2d -> Int -> S.HashSet Point2d -> S.HashSet Point2d
step g 0 ps = ps
step g i ps = step g (pred i) $ S.filter isGarden $ S.fromList $ concatMap point2dNeighbours ps
  where
    -- hardcoded input grid size for actual input
    size = 131
    isGarden (x, y) = (x `mod` size, y `mod` size) `S.member` g

parseInput :: String -> (S.HashSet Point2d, Point2d)
parseInput input = (S.fromList $ map fst g, s)
  where
    Just s = fst <$> find ((== 'S') . snd) g
    g =
      [ ((c, r), v)
        | (r, vs) <- zip [0 ..] $ lines input,
          (c, v) <- zip [0 ..] vs,
          v /= '#'
      ]
