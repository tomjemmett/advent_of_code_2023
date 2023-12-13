module Day13Spec (spec) where

import SpecHelper

testInput =
  "#.##..##.\n\
  \..#.##.#.\n\
  \##......#\n\
  \##......#\n\
  \..#.##.#.\n\
  \..##..##.\n\
  \#.#.##.#.\n\
  \\n\
  \#...##..#\n\
  \#....#..#\n\
  \..##..###\n\
  \#####.##.\n\
  \#####.##.\n\
  \..##..###\n\
  \#....#..#\n\
  \\n"

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
    day13 testInput `shouldBe` ["405", "400"]

  it "Actual" $ do
    withFile
      "inputs/day13.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day13 actualInput `shouldBe` ["37113", "30449"]
      )