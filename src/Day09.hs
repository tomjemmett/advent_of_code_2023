module Day09 (day09) where

import Common
import Control.Monad (ap)
import Text.Parsec qualified as P

type Input = [[Int]]

day09 :: AOCSolution
day09 = map show . ap [part1, part2] . pure . parseInput

part1 :: Input -> Int
part1 = sum . map go
part2 :: Input -> Int
part2 = part1 . map reverse

go :: [Int] -> Int
go x = last x + if all (== d) x' 
  then d 
  else go x'
  where
    x'@(d:_) = zipWith (-) (drop 1 x) x

parseInput :: String -> Input
parseInput = parseLinesWith (P.many number)
