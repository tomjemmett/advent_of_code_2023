{-# OPTIONS_GHC -Wno-x-partial #-}

module Day01 (day01) where

import Common
import Data.Char (digitToInt)
import Data.Either (either, lefts)
import Data.Maybe (catMaybes)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)


day01 :: AOCSolution
day01 input = map (show . sum . map f) $ map <$> [lefts, map (either id id)] <*> pure (parseInput input)
  where
    f xs@(x:_) = x * 10 + last xs

parseInput :: String -> [[Either Int Int]]
parseInput = map (parse' p id) . lines
  where
    p :: Parser [Either Int Int]
    p = catMaybes <$> (P.many $ P.choice $ P.try <$> pEdgeCases ++ pNumbers ++ [pDigit, pFail])
    -- 
    pEdgeCases :: [Parser (Maybe (Either Int Int))]
    pEdgeCases = map pEdgeCase [
      (1, "on", "eight"),
      (2, "tw", "one"),
      (5, "fiv", "eight"),
      (7, "seve", "nine"),
      (8, "eigh", "two"),
      (8, "eigh", "three"),
      (9, "nin", "eight")
      ]
    pEdgeCase :: (Int, String, String) -> Parser (Maybe (Either Int Int))
    pEdgeCase (n, i, j) = do
      P.string i
      P.lookAhead $ P.string j
      pure $ Just (Right n)
    --
    pNumbers :: [Parser (Maybe (Either Int Int))]
    pNumbers = [
      pNumber x |
      x <- zip [1..] ["one"
                    ,"two"
                    ,"three"
                    ,"four"
                    ,"five"
                    ,"six"
                    ,"seven"
                    ,"eight"
                    ,"nine"]]
    pNumber :: (Int, String) -> Parser (Maybe (Either Int Int))
    pNumber (n, s) = Just (Right n) <$ P.string s
    --
    pDigit :: Parser (Maybe (Either Int Int))
    pDigit = Just . Left . digitToInt <$> P.digit
    --
    pFail :: Parser (Maybe (Either Int Int))
    pFail = Nothing <$ P.anyChar
