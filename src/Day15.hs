module Day15 (day15) where

import Common
import Control.Lens
import Control.Monad (ap)
import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Vector qualified as V
import Text.Parsec qualified as P

day15 :: AOCSolution
day15 = map show . ap [part1, part2] . pure

data Operation = Insert Int | Remove deriving (Show)

hash :: String -> Int
hash = foldl (\a x -> (a + ord x) * 17 `mod` 256) 0

part1 :: String -> Int
part1 = sum . map hash . splitOn ","

part2 :: String -> Int
part2 = solve . filter (not . null . snd) . zip [1 ..] . map score . V.toList . foldl go v . p
  where
    solve vss = sum $ [m * i | (m, vs) <- vss, i <- vs]
    score = zipWith (*) [1 ..] . map snd
    v = V.fromList $ replicate 256 []
    p = parse $ flip P.sepBy (P.char ',') do
      k <- P.many P.letter
      o <- P.try pInsert P.<|> pRemove
      pure (k, o)
      where
        pInsert = Insert <$> (P.char '=' *> number)
        pRemove = Remove <$ P.char '-'

go :: V.Vector [(String, Int)] -> (String, Operation) -> V.Vector [(String, Int)]
go v (k, op) =
  v
    & ix h .~ case op of
      Insert i -> ins i $ v V.! h
      Remove -> filter ((/= k) . fst) $ v V.! h
  where
    h = hash k
    ins :: Int -> [(String, Int)] -> [(String, Int)]
    ins i [] = [(k, i)]
    ins i (x : xs) =
      if fst x == k
        then (k, i) : xs
        else x : ins i xs
