module Day21 (day21) where

import Common
import Data.HashSet qualified as S
import Data.List (find)

gridSize :: Int
gridSize = 131

day21 :: AOCSolution
day21 input = show <$> [p1, p2]
  where
    (g, s) = parseInput input
    r = map S.size $ iterate (step g) $ S.singleton s
    p1 = r !! 64
    p2 = solve r

solve :: [Int] -> Int
solve r = a0 + (a1 - a0) * n + (n * pred n `div` 2) * (a2 - 2 * a1 + a0)
  where
    n = 26501365 `div` gridSize
    [a0, a1, a2] = (r !!) <$> [65 + i * gridSize | i <- [0 .. 2]]

step :: S.HashSet Point2d -> S.HashSet Point2d -> S.HashSet Point2d
step g ps = S.filter isGarden $ S.fromList $ concatMap point2dNeighbours ps
  where
    isGarden = not . flip S.member g . flip both (`mod` gridSize)

parseInput :: String -> (S.HashSet Point2d, Point2d)
parseInput input = (S.delete s $ S.fromList $ map fst g, s)
  where
    Just s = fst <$> find ((== 'S') . snd) g
    g =
      [ ((c, r), v)
        | (r, vs) <- zip [0 ..] $ lines input,
          (c, v) <- zip [0 ..] vs,
          v /= '.'
      ]
