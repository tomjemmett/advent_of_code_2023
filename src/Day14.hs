module Day14 (day14) where

import Common
import Control.Monad (ap)
import Data.Array qualified as A
import Data.Foldable qualified as F (toList)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)

type Input = A.Array Point2d Char

day14 :: AOCSolution
day14 = map show . ap [part1, part2] . pure . parseInput

part1, part2 :: Input -> Int
part1 = solve . flip roll 'N'
part2 = findRepeatingPattern 1000000000 spinCycle F.toList solve

solve :: Input -> Int
solve i = sum do
  let ((minr, minc), (maxr, maxc)) = A.bounds i
  r <- [minr .. maxr]
  c <- [minc .. maxc]
  pure $ if i A.! (r, c) == 'O' then r else 0

spinCycle :: Input -> Input
spinCycle i = foldl roll i "NWSE"

roll :: Input -> Char -> Input
roll i c =
  i'
    A.// concat
      ( f $ case c of
          'N' -> ([(maxr, c) | c <- [minc .. maxc]], (-1, 0))
          'E' -> ([(r, maxc) | r <- [minr .. maxr]], (0, -1))
          'S' -> ([(minr, c) | c <- [minc .. maxc]], (1, 0))
          'W' -> ([(r, minc) | r <- [minr .. maxr]], (0, 1))
      )
  where
    b@((minr, minc), (maxr, maxc)) = A.bounds i
    f (xs, d) = map (\x -> roll' x x d []) xs
    i' = fmap (\v -> if v == '#' then '#' else '.') i
    roll' :: Point2d -> Point2d -> Point2d -> [(Point2d, Char)] -> [(Point2d, Char)]
    roll' n@(x, y) p@(r, c) d@(dr, dc) a =
      if not $ A.inRange b p
        then a
        else roll' n' p' d a'
      where
        v = i A.! p
        p' = (r + dr, c + dc)
        n' = case v of
          '.' -> n
          '#' -> p'
          'O' -> (x + dr, y + dc)
        a' = if v == 'O' then (n, 'O') : a else a

toString :: Input -> String
toString i = unlines do
  let ((minr, minc), (maxr, maxc)) = A.bounds i
  r <- reverse [minr .. maxr]

  pure [i A.! (r, c) | c <- [minc .. maxc]]

parseInput :: String -> Input
parseInput (lines -> i) =
  A.array
    b
    [ ((r, c), v)
      | (r, l) <- zip [rows, rows - 1 ..] i,
        (c, v) <- zip [1 ..] l
    ]
  where
    rows = length i
    cols = length $ head i
    b = ((1, 1), (rows, cols))
