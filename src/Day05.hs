module Day05 (day05) where

import Common
import Control.Monad (ap)
import Debug.Trace (traceM)
import Text.Parsec qualified as P

type Input = ([Int], [[(Int, Int, Int)]])

day05 :: AOCSolution
day05 = map show . ap [part1, part2] . pure . parseInput

part1 :: Input -> Int
part1 (s, m) = minimum $ map (`go` m) s

part2 :: Input -> Int
part2 = minimum . p2
  where
    p2 ([], _) = []
    p2 (s1 : s2 : ss, m) = part1 ([s1 .. (s1 + s2 - 1)], m) : p2 (ss, m)

-- assume maps go a->b, b->c, c->d ...
parseInput :: String -> Input
parseInput = (`parse'` id) $ do
  seeds <- P.string "seeds: " *> P.sepBy number (P.char ' ')
  P.newline
  P.newline
  maps <- (`P.sepEndBy` P.newline) $ do
    P.many P.letter
    P.string "-to-"
    P.many P.letter
    P.string " map:"
    P.newline
    --
    (`P.sepEndBy` P.newline) $ do
      d <- number <* P.char ' '
      s <- number <* P.char ' '
      n <- number
      pure (d, s, n)
  pure (seeds, maps)

go :: Int -> [[(Int, Int, Int)]] -> Int
go i [] = i
go i (m : ms) = go (f m) ms
  where
    f [] = i
    f ((d, s, n) : xs) =
      if i >= s && i < s + n
        then i - s + d
        else f xs
