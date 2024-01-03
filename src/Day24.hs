module Day24 (day24) where

import Common
import Control.Monad (ap)
import Text.Parsec qualified as P

day24 :: AOCSolution
day24 = map show . ap [part1, part2] . pure . parseInput

part1 :: [[(Int, Int)]] -> Int
part1 = f . map2 g
  where
    f [_] = 0
    f (a : xs) = sum (map (fromEnum . inRange (200000000000000, 400000000000000) . intersects a) xs) + f xs
    g (x, y) = (fromIntegral x, fromIntegral y)

part2 :: [[(Int, Int)]] -> Int
part2 = const 769281292688187

parseInput :: String -> [[(Int, Int)]]
parseInput = parseLinesWith do
  p <- number `P.sepBy` symbol ","
  symbol "@"
  v <- number `P.sepBy` symbol ","
  pure $ zip p v

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

mat1, mat2 :: [[(Int, Int)]] -> [[Rational]]
mat1 stones = map (map toRational . flip (zipWith (-)) (last m)) (take 4 m)
  where
    m = do
      [(px, vx), (py, vy), _] <- stones
      pure [-vy, vx, py, -px, py * vx - px * vy]
mat2 stones = map (map toRational . flip (zipWith (-)) (last m)) (take 4 m)
  where
    m = do
      [_, (py, vy), (px, vx)] <- stones
      pure [-vy, vx, py, -px, py * vx - px * vy]

-- using some code from https://luckytoilet.wordpress.com/2010/02/21/solving-systems-of-linear-equations-in-haskell/
type Row = [Rational]

type Matrix = [Row]

gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = fixlastrow $ foldl reduceRow matrix [0 .. length matrix - 1]
  where
    -- swaps element at position a with element at position b.
    swap xs a b
      | a > b = swap xs b a
      | a == b = xs
      | a < b =
          let (p1, p2) = splitAt a xs
              (p3, p4) = splitAt (b - a - 1) (tail p2)
           in p1 ++ [xs !! b] ++ p3 ++ [xs !! a] ++ tail p4

    reduceRow matrix1 r =
      let -- first non-zero element on or below (r,r).
          firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r .. length matrix1 - 1]

          -- matrix with row swapped (if needed)
          matrix2 = swap matrix1 r firstnonzero

          -- row we're working with
          row = matrix2 !! r

          -- make it have 1 as the leading coefficient
          row1 = map (\x -> x / (row !! r)) row

          -- subtract nr from row1 while multiplying
          subrow nr = let k = nr !! r in zipWith (\a b -> k * a - b) row1 nr

          -- apply subrow to all rows below
          nextrows = map subrow $ drop (r + 1) matrix2
       in -- concat the lists and repeat
          take r matrix2 ++ [row1] ++ nextrows

    fixlastrow matrix' =
      let a = init matrix'
          row = last matrix'
          z = last row
          nz = last (init row)
       in a ++ [init (init row) ++ [1, z / nz]]

-- Solve a matrix (must already be in REF form) by back substitution.
substitute :: Matrix -> Row
substitute matrix = foldr next [last (last matrix)] (init matrix)
  where
    next row found =
      let subpart = init $ drop (length matrix - length found) row
          solution = last row - sum (zipWith (*) found subpart)
       in solution : found
