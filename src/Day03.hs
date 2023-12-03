module Day03 (day03) where

import Common
import Control.Monad (ap)
import Data.Char (isDigit)
import Data.HashMap.Strict qualified as M
import Data.List (nub)
import Data.Maybe (mapMaybe)

data Schematic = Symbol Char | PartNumber Point2d Int deriving (Eq, Show)

day03 :: AOCSolution
day03 = map show . ap [part1, part2] . pure . parseInput

part1 :: M.HashMap Point2d Schematic -> Int
part1 i = sum n
  where
    s = M.keys $ M.filter isSymbol i
    n = map snd $ nub $ concatMap (findAdjacentPartNumbers i) s

part2 :: M.HashMap Point2d Schematic -> Int
part2 i = sum $ map product n
  where
    s = M.keys $ M.filter isGear i
    n = map2 snd $ filter ((== 2) . length) $ map (findAdjacentPartNumbers i) s

findAdjacentPartNumbers :: M.HashMap Point2d Schematic -> Point2d -> [(Point2d, Int)]
findAdjacentPartNumbers i p = map fromPartNumber $ nub $ mapMaybe (`M.lookup` i) n
  where
    n = point2dNeighboursDiags p

parseInput :: String -> M.HashMap Point2d Schematic
parseInput = M.fromList . concat . zipWith (\n s -> parseLine n (0, s)) [0 ..] . lines

parseLine :: Int -> (Int, String) -> [(Point2d, Schematic)]
parseLine _ (_, []) = []
parseLine n (i, xss@(x : xs))
  | isDigit x = [((i + i', n), PartNumber (i, n) $ read engine) | i' <- [0 .. ln - 1]] ++ parseLine n (i + ln, drop ln xss)
  | x == '.' = parseLine n (i + 1, xs)
  | otherwise = ((i, n), Symbol x) : parseLine n (i + 1, xs)
  where
    engine = takeWhile isDigit xss
    ln = length engine

fromPartNumber :: Schematic -> (Point2d, Int)
fromPartNumber (PartNumber p n) = (p, n)

isSymbol :: Schematic -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isGear :: Schematic -> Bool
isGear (Symbol '*') = True
isGear _ = False
