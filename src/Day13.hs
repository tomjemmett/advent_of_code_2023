module Day13 (day13) where

import Common
import Control.Monad (ap)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Vector qualified as V

type Input = [(Int, V.Vector String)]

day13 :: AOCSolution
day13 = map show . ap (go <$> [0, 1]) . pure . parseInput

go :: Int -> Input -> Int
go n = sum . mapMaybe (findReflection n)

findReflection :: Int -> (Int, V.Vector String) -> Maybe Int
findReflection d (m, i) = f [l `div` 2]
  where
    l = length i
    f :: [Int] -> Maybe Int
    f [] = Nothing
    f (n : ns)
      | null gl = Nothing
      | null gr = Nothing
      | sum (zipWith hammingDistance gl gr) == d = Just $ m * (n + 1)
      | otherwise = f (ns ++ [n - 1, n + 1])
      where
        gl = [i V.! x | x <- [n, n - 1 .. 0]]
        gr = [i V.! x | x <- [(n + 1) .. l - 1]]

parseInput :: String -> [(Int, V.Vector String)]
parseInput = concatMap (f . lines) . splitOn "\n\n"
  where
    f ls = [(100, V.fromList ls), (1, V.fromList $ transpose ls)]
