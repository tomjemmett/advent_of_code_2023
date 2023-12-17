module Day17 (day17) where

import Common
import Control.Monad (ap, guard)
import Data.Array qualified as A
import Data.Char (digitToInt)
import PathFinding

type Input = A.Array Point2d Int

type Movement = (Point2d, Char)

day17 :: AOCSolution
day17 = map show . ap (go <$> [(1, 3), (4, 10)]) . pure . parseInput

go :: (Int, Int) -> Input -> Int
go n i = fst . last $ unsafeFindPath (move t n i) ((0, 0), 'I') (t, 'G')
  where
    (_, t) = A.bounds i

move :: Point2d -> (Int, Int) -> Input -> Movement -> [(Int, Movement)]
move target (minDist, maxDist) i (p@(x, y), d) =
  if p == target
    then [(0, (p, 'G'))]
    else moves
  where
    inRange = A.inRange (A.bounds i)
    moves = case d of
      'I' -> moveS ++ moveE
      'N' -> moveE ++ moveW
      'S' -> moveE ++ moveW
      'E' -> moveN ++ moveS
      'W' -> moveN ++ moveS
    moveN = do
      y' <- [minDist .. maxDist]
      let p' = (x, y - y')
      guard $ inRange p'
      let v = sum [i A.! (x, y - y'') | y'' <- [1 .. y']]
      pure (v, (p', 'N'))
    moveS = do
      y' <- [minDist .. maxDist]
      let p' = (x, y + y')
      guard $ inRange p'
      let v = sum [i A.! (x, y + y'') | y'' <- [1 .. y']]
      pure (v, (p', 'S'))
    moveE = do
      x' <- [minDist .. maxDist]
      let p' = (x + x', y)
      guard $ inRange p'
      let v = sum [i A.! (x + x'', y) | x'' <- [1 .. x']]
      pure (v, (p', 'E'))
    moveW = do
      x' <- [minDist .. maxDist]
      let p' = (x - x', y)
      guard $ inRange p'
      let v = sum [i A.! (x - x'', y) | x'' <- [1 .. x']]
      pure (v, (p', 'E'))

parseInput :: String -> Input
parseInput input = A.array ((0, 0), (cn - 1, rn - 1)) g
  where
    i = lines input
    rn = length i
    cn = length $ head i
    g = [((c, r), digitToInt v) | (r, vs) <- zip [0 ..] i, (c, v) <- zip [0 ..] vs]
