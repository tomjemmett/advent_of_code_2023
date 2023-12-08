module Day08 (day08) where

import Common
import Control.Monad (ap)
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

type Input = ([(String, String) -> String], M.HashMap String (String, String))

day08 :: AOCSolution
day08 = map show . ap [part1, part2] . pure . parseInput

go :: (String -> Bool) -> String -> Input -> Int
go d s (i, m) = f (cycle i) s
  where
    f (x : xs) k =
      if d k
        then 0
        else 1 + f xs (x (m M.! k))

part1 :: Input -> Int
part1 = go (== "ZZZ") "AAA"

part2 :: Input -> Int
part2 (i, m) = foldl1 lcm . map (flip (go d) (i, m)) $ k 'A'
  where
    k x = filter ((== x) . last) $ M.keys m
    d = flip elem (k 'Z')

parseInput :: String -> Input
parseInput = parse do
  let f 'L' = fst
      f 'R' = snd
  i <- P.many (P.oneOf "LR")
  P.many P.newline
  m <- flip P.sepEndBy P.newline $ do
    k <- P.many P.alphaNum <* P.string " = ("
    l <- P.many P.alphaNum <* P.string ", "
    r <- P.many P.alphaNum <* P.char ')'
    pure (k, (l, r))
  pure (map f i, M.fromList m)