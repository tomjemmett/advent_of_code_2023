module Day12 (day12) where

import Common
import Control.Monad (ap)
import Data.Array qualified as A
import Data.List (intercalate)
import Data.List.Split (splitOn)

day12 :: AOCSolution
day12 = map show . ap [part1, part2] . pure . parseInput

part1, part2 :: [(String, [Int])] -> Int
part1 = sum . map go
part2 = part1 . map unfold
  where
    unfold :: (String, [Int]) -> (String, [Int])
    unfold (xs, ys) = (intercalate "?" (replicate 5 xs), concat $ replicate 5 ys)

parseInput :: String -> [(String, [Int])]
parseInput = map ((\[a, b] -> (a, commaSeparatedInts b)) . splitOn " ") . lines

go :: (String, [Int]) -> Int
go (conditionRecord, groupSizes) = valids conditionRecord groupSizes
  where
    lcr = length conditionRecord
    lgs = length groupSizes

    v =
      A.array
        ((0, 0), (lcr, lgs))
        [ ((x, y), f (drop x conditionRecord) (drop y groupSizes))
          | x <- [0 .. lcr],
            y <- [0 .. lgs]
        ]

    valids :: [Char] -> [Int] -> Int
    valids cr gs = v A.! (lcr - length cr, lgs - length gs)

    f :: [Char] -> [Int] -> Int
    f [] [] = 1
    f [] _ = 0
    f ('#' : _) [] = 0
    f ('.' : xs) ys = valids xs ys
    f xs@('#' : _) (y : ys)
      | length xs < y = 0
      | '.' `elem` take y xs = 0
      | length xs > y && xs !! y == '#' = 0
      | otherwise = valids (drop (y + 1) xs) ys
    f ('?' : xs) ys = valids xs ys + f ('#' : xs) ys
