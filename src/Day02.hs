module Day02 (day02) where

import Common
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

day02 :: AOCSolution
day02 input = show <$> ([part1, part2] <*> pure (parseInput input))

part2 :: [(Int, [[(Int, String)]])] -> Int
part2 = sum . map (power . maxEachColour)
  where
    power :: M.HashMap String Int -> Int
    power = product . map snd . M.toList

    maxEachColour :: (Int, [[(Int, String)]]) -> M.HashMap String Int
    maxEachColour = foldr (uncurry (flip $ M.insertWith max)) m . concat . snd

    m :: M.HashMap String Int
    m = M.fromList [("red", 0), ("green", 0), ("blue", 0)]

part1 :: [(Int, [[(Int, String)]])] -> Int
part1 = foldr ((+) . gameValid) 0
  where
    gameValid :: (Int, [[(Int, String)]]) -> Int
    gameValid (gId, grabs) = if all grabValid grabs then gId else 0

    grabValid :: [(Int, String)] -> Bool
    grabValid = all f

    f :: (Int, String) -> Bool
    f (i, colour) = case colour of
      "red" -> i <= 12
      "green" -> i <= 13
      "blue" -> i <= 14

parseInput :: String -> [(Int, [[(Int, String)]])]
parseInput = map (parse' p id) . lines
  where
    p = do
      gId <- P.string "Game " *> number
      P.string ": "
      grabs <- pGrab `P.sepBy` P.string "; "
      pure (gId, grabs)
    pGrab = pCube `P.sepBy` P.string ", "
    pCube = do
      n <- number
      P.char ' '
      c <- P.many P.letter
      pure (n, c)
