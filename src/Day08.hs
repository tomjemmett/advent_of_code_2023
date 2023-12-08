module Day08 (day08) where

import Common
import Control.Monad (ap)
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

type Input = (String, M.HashMap String (String, String))

day08 :: AOCSolution
day08 = map show . ap [part1, part2] . pure . parseInput

go :: String -> (String -> Bool) -> Input -> Int
go s d (i, m) = f (cycle i) s
  where
    f :: String -> String -> Int
    f (x : xs) k =
      if d k
        then 0
        else 1 + f xs ((if x == 'L' then fst else snd) (m M.! k))

part1 :: Input -> Int
part1 = go "AAA" (== "ZZZ")

part2 :: Input -> Int
part2 (i, m) = foldl1 lcm $ map (\x -> go x d (i, m)) as
  where
    k = M.keys m
    as = filter ((== 'A') . last) k
    zs = filter ((== 'Z') . last) k
    d x = x `elem` zs

parseInput :: String -> Input
parseInput = parse do
  i <- P.many $ P.oneOf "LR"
  P.many P.newline
  m <- flip P.sepEndBy P.newline $ do
    k <- P.many P.alphaNum
    P.string " = ("
    l <- P.many P.alphaNum
    P.string ", "
    r <- P.many P.alphaNum
    P.char ')'
    pure (k, (l, r))
  pure (i, M.fromList m)