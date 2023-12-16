module Day16 (day16) where

import Common
import Control.Monad (ap, forM_, unless)
import Control.Monad.State (MonadState (get), State, execState, modify)
import Data.Array qualified as A
import Data.HashSet qualified as S
import Data.List (nub)

type Input = A.Array Point2d Char

day16 :: AOCSolution
day16 = map show . ap [part1, part2] . pure . parseInput

search :: Input -> (Point2d, Point2d) -> State (S.HashSet (Point2d, Point2d)) ()
search g pd@(p, d) = do
  s <- get
  unless (pd `S.member` s) do
    let t = g A.! p
    modify $ S.insert pd
    forM_ (filter (A.inRange (A.bounds g) . fst) $ move (p, d) t) (search g)

move :: (Point2d, Point2d) -> Char -> [(Point2d, Point2d)]
move (p@(y, x), d@(dy, dx)) = f
  where
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
go i pd = length $ nub $ map fst $ S.toList $ execState (search i pd) mempty

part1, part2 :: Input -> Int
part1 i = go i ((1, 1), (0, 1))
part2 i = maximum $ map (go i) starts
  where
    ((minr, minc), (maxr, maxc)) = A.bounds i
    starts :: [(Point2d, Point2d)]
    starts =
      concat
        [ [((minr, c), (1, 0)) | c <- [minc .. maxc]],
          [((maxr, c), (-1, 0)) | c <- [minc .. maxc]],
          [((r, minc), (0, 1)) | r <- [minr .. maxr]],
          [((r, maxc), (0, -1)) | r <- [minr .. maxr]]
        ]

parseInput :: String -> Input
parseInput input = A.listArray ((1, 1), (nr, nc)) a
  where
    i = lines input
    nr = length i
    nc = length $ head i
    a = [v | vs <- i, v <- vs]
