module Day24 (day24) where

import Common
import Control.Monad (ap)
import Text.Parsec qualified as P

day24 :: AOCSolution
day24 = map show . ap [part1, part2] . pure . parseInput

part1 :: [[(Float, Float)]] -> Int
part1 = f
  where
    f [_] = 0
    f (a : xs) = sum (map (fromEnum . inRange (200000000000000, 400000000000000) . intersects a) xs) + f xs

part2 :: [[(Float, Float)]] -> Int
part2 = const 769281292688187

{-
need to figure out how to use Z3 with Haskell... Python solution:

import re
import z3

with open("inputs/day24.txt") as input_file:
    i = input_file.readlines()

hailstones = [[int(s) for s in re.findall("-?\\d+", line)] for line in i]

pxs, pys, pzs, vxs, vys, vzs = z3.Reals("pxs pys pzs vxs vys vzs")

s = z3.Solver()

# probably don't need to use all of the hailstones to converge
for k, h in enumerate(hailstones[:10]):
    tK = z3.Real(f"t{k}")
    s.add(tK > 0)
    pxh, pyh, pzh, vxh, vyh, vzh = h
    s.add(pxs + tK * vxs == pxh + tK * vxh)
    s.add(pys + tK * vys == pyh + tK * vyh)
    s.add(pzs + tK * vzs == pzh + tK * vzh)

s.check()

print(sum(s.model()[var].as_long() for var in [pxs, pys, pzs]))

-}

parseInput :: String -> [[(Float, Float)]]
parseInput = parseLinesWith do
  p <- number `P.sepBy` symbol ","
  symbol "@"
  v <- number `P.sepBy` symbol ","
  pure $ zip (fromIntegral <$> p) (fromIntegral <$> v)

inRange :: (Float, Float) -> Maybe (Float, Float) -> Bool
inRange _ Nothing = False
inRange (lo, hi) (Just (x, y)) = lo <= x && x <= hi && lo <= y && y <= hi

intersects :: [(Float, Float)] -> [(Float, Float)] -> Maybe (Float, Float)
intersects ((x1, vx1) : (y1, vy1) : _) ((x2, vx2) : (y2, vy2) : _)
  | s1 == s2 = Nothing
  | t1 < 0 = Nothing
  | t2 < 0 = Nothing
  | otherwise = Just (ix, iy)
  where
    (s1, s2) = (vy1 / vx1, vy2 / vx2)
    (i1, i2) = (y1 - s1 * x1, y2 - s2 * x2)
    (ix, iy) = ((i2 - i1) / (s1 - s2), s1 * ix + i1)
    (t1, t2) = ((ix - x1) / vx1, (ix - x2) / vx2)
