{-# OPTIONS_GHC -Wno-x-partial #-}

module Day01 (day01) where

import Common
import Data.Char (digitToInt)
import Data.Either (either, lefts)
import Data.Maybe (catMaybes)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day01 :: AOCSolution
day01 input =
  map (show . sum . map f) $
    map
      <$> [lefts, map (either id id)]
      <*> pure (parseInput input)
  where
    f xs@(x : _) = x * 10 + last xs

parseInput :: String -> [[Either Int Int]]
parseInput = map (parse' p id) . lines
  where
    p :: Parser [Either Int Int]
    p = catMaybes <$> P.many (P.choice $ P.try <$> pNumbers ++ [pDigit, pFail])
    --
    pNumbers :: [Parser (Maybe (Either Int Int))]
    pNumbers = do
      (n, s) <- zip [1 ..] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      pure $ Just (Right n) <$ (P.string (init s) >> P.lookAhead (P.char $ last s))
    --
    pDigit :: Parser (Maybe (Either Int Int))
    pDigit = Just . Left . digitToInt <$> P.digit
    --
    pFail :: Parser (Maybe (Either Int Int))
    pFail = Nothing <$ P.anyChar
