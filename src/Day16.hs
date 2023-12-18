module Day16 (day16) where

import Common
import Control.Monad (ap)
import Data.Array qualified as A
import Data.HashSet qualified as S

type Input = A.Array Point2d Char

day16 :: AOCSolution
day16 = map show . ap [part1, part2] . pure . parseInput

search :: Input -> [(Point2d, Point2d)] -> S.HashSet (Point2d, Point2d) -> Int
search g [] v = countTrue ((/= '@') . (g A.!)) . S.map fst $ v
search g (pd@(p, d) : ps) v =
  if pd `S.member` v
    then search g ps v
    else search g ps' v'
  where
    v' = S.insert pd v
    t = g A.! p
    ps' = move (p, d) t ++ ps

move :: (Point2d, Point2d) -> Char -> [(Point2d, Point2d)]
move (p@(y, x), d@(dy, dx)) = f
  where
    f '@' = []
    f '.' = [((y + dy, x + dx), d)]
    f '#' = [((y + dy, x + dx), d)]
    f '|'
      | dx == 0 = [((y + dy, x + dx), d)]
      | otherwise = [((y - 1, x), (-1, 0)), ((y + 1, x), (1, 0))]
    f '-'
      | dy == 0 = [((y + dy, x + dx), d)]
      | otherwise = [((y, x - 1), (0, -1)), ((y, x + 1), (0, 1))]
    f '/'
      | dy == 0 =
          if dx == 1
            then [((y - 1, x), (-1, 0))]
            else [((y + 1, x), (1, 0))]
      | dx == 0 =
          if dy == 1
            then [((y, x - 1), (0, -1))]
            else [((y, x + 1), (0, 1))]
    f '\\'
      | dy == 0 =
          if dx == 1
            then [((y + 1, x), (1, 0))]
            else [((y - 1, x), (-1, 0))]
      | dx == 0 =
          if dy == 1
            then [((y, x + 1), (0, 1))]
            else [((y, x - 1), (0, -1))]

go :: Input -> (Point2d, Point2d) -> Int
go i pd = search i [pd] mempty

part1, part2 :: Input -> Int
part1 i = go i ((1, 1), (0, 1))
part2 i = maximum $ map (go i) starts
  where
    ((minr, minc), (maxr, maxc)) = A.bounds i
    starts :: [(Point2d, Point2d)]
    starts =
      concat
        [ [((succ minr, c), (1, 0)) | c <- [minc .. maxc]],
          [((pred maxr, c), (-1, 0)) | c <- [minc .. maxc]],
          [((r, succ minc), (0, 1)) | r <- [minr .. maxr]],
          [((r, pred maxc), (0, -1)) | r <- [minr .. maxr]]
        ]

parseInput :: String -> Input
parseInput input = A.listArray ((0, 0), (nr + 1, nc + 1)) (repeat '@') A.// a
  where
    i = lines input
    nr = length i
    nc = length $ head i
    a = [((r, c), v) | (r, vs) <- zip [1 ..] i, (c, v) <- zip [1 ..] vs]
