module Day05 (day05) where

import Common
import Control.Monad (ap)
import Data.Function (on)
import Data.List (elemIndex)
import Text.Parsec qualified as P

type Input = ([Int], [[(Int, Int, Int)]])

day05 :: AOCSolution
day05 = map show . ap [part1, part2] . pure . parseInput

part1 :: Input -> Int
part1 (s, m) = minimum $ map (go m) s

part2 :: Input -> Int
part2 (s, m) = minimum $ p2 s
  where
    p2 [] = []
    p2 (s1 : s2 : ss) = part1 ([l .. r], m) : p2 ss
      where
        seeds = [s1, s1 + floor (sqrt $ fromIntegral s2) .. s1 + s2]
        locations = map (go m) seeds
        Just n = elemIndex (minimum locations) locations
        [l, r] = take 2 $ drop (n - 1) seeds

-- assume maps go a->b, b->c, c->d ...
parseInput :: String -> Input
parseInput = parse $ do
  seeds <- symbol "seeds:" *> P.many number
  maps <- (`P.sepEndBy` P.newline) $ do
    P.manyTill P.anyChar (symbol ":")
    --
    (`P.sepEndBy` P.newline) $ do
      d <- number
      s <- number
      n <- number'
      pure (d, s, n)
  pure (seeds, maps)

go :: [[(Int, Int, Int)]] -> Int -> Int
go = flip (foldl f)
  where
    f i [] = i
    f i ((d, s, n) : xs) =
      if i >= s && i < s + n
        then i - s + d
        else f i xs
