module Day02 (day02) where

import Common
import Control.Monad (ap)
import Text.Parsec qualified as P

day02 :: AOCSolution
day02 = map show . ap [part1, part2] . pure . parseInput

part1, part2 :: [(Int, [Int])] -> Int
part1 = sum . map gameValid
  where
    gameValid (gId, [r, g, b])
      | r > 12 = 0
      | g > 13 = 0
      | b > 14 = 0
      | otherwise = gId
part2 = sum . map (product . snd)

parseInput :: String -> [(Int, [Int])]
parseInput = parseLines $ do
  -- strip the id out between the text "Game " and ":"
  gId <- P.between (symbol "Game") (P.char ':') number
  -- get all of the colour/values for the entire game
  grabs <- pCube `P.sepBy` P.oneOf ",;"
  -- return the game and the values, reducing the R/G/B values to the max
  -- value for the current game
  pure (gId, foldr f [0, 0, 0] grabs)
  where
    pCube = do
      -- each cube starts with a space, followed by a number, followed by a
      -- space, but we only care about keeping the number
      n <- P.space *> number
      -- get the name of the colour, which will be made up of letters only
      c <- P.many P.letter
      -- return the number and the colour
      pure (n, c)
    -- take a number and a colour, and the current R/G/B values, and alter the
    -- value for that colour if the new value is bigger
    f (n, c) [r, g, b] = case c of
      "red" -> [max r n, g, b]
      "green" -> [r, max g n, b]
      "blue" -> [r, g, max b n]
