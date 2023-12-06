module Day06 where -- (day06) where

import Common
import Text.Parsec qualified as P

day06 :: AOCSolution
day06 input = map show [product . map solve $ p1, solve p2]
  where
    (p1, p2) = parseInput input

solve :: (Int, Int) -> Int
solve (fromIntegral -> t, fromIntegral -> d) = maxT - minT + 1
  where
    x = sqrt $ t ^ 2 - 4 * (d + 1)
    maxT = floor ((t + x) / 2)
    minT = ceiling ((t - x) / 2)

parseInput :: String -> ([(Int, Int)], (Int, Int))
parseInput = parse $ do
  t <- symbol "Time:" *> P.many (lexeme $ P.many1 P.digit)
  d <- symbol "Distance:" *> P.many (lexeme $ P.many1 P.digit)
  pure (zip (map read t) (map read d), (read $ concat t, read $ concat d))
