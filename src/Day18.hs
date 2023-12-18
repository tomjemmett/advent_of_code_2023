module Day18 (day18) where

import Common
import Control.Monad (ap)
import Text.Parsec qualified as P

type Input = [(Char, Int)]

day18 :: AOCSolution
day18 = map show . ap [part1, part2] . pure . parseInput

part1, part2 :: (Input, Input) -> Int
part1 = area . fst
part2 = area . snd

area :: Input -> Int
area i = picks e l + l
  where
    e = getEdge i
    l = getLength i

-- https://en.wikipedia.org/wiki/Pick%27s_theorem
picks :: [Point2d] -> Int -> Int
picks edges len = shoelace edges + 1 - (len `div` 2)

-- https://en.wikipedia.org/wiki/Shoelace_formula
shoelace :: [Point2d] -> Int
shoelace xs = (`div` 2) . abs $ f (last xs : xs)
  where
    f [_] = 0
    f ((x1, y1) : p@(x2, y2) : xs) = x1 * y2 - x2 * y1 + f (p : xs)

getLength :: Input -> Int
getLength = foldr ((+) . snd) 0

getEdge :: Input -> [Point2d]
getEdge = fst . foldl g ([], (0, 0))
  where
    g :: ([Point2d], Point2d) -> (Char, Int) -> ([Point2d], Point2d)
    g (m, p@(x, y)) (d, n) = (p' : m, p')
      where
        p' = case d of
          'L' -> (x - n, y)
          'R' -> (x + n, y)
          'U' -> (x, y - n)
          'D' -> (x, y + n)

parseInput :: String -> (Input, Input)
parseInput =
  unzip . parseLinesWith do
    d <- lexeme $ P.oneOf "LRUD"
    n <- number
    c <- (P.char '(' `P.between` P.char ')') do
      P.char '#'
      a <- P.count 5 P.alphaNum
      b <- P.digit
      let d = case b of
            '0' -> 'R'
            '1' -> 'D'
            '2' -> 'L'
            '3' -> 'U'
      pure (d, read $ "0x" ++ a)
    pure ((d, n), c)
