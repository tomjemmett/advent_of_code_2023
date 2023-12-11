module Day11Spec (spec) where

import SpecHelper

testInput =
  "...#......\n\
  \.......#..\n\
  \#.........\n\
  \..........\n\
  \......#...\n\
  \.#........\n\
  \.........#\n\
  \..........\n\
  \.......#..\n\
  \#...#....."

spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
    day11 testInput `shouldBe` ["374","82000210"]

  it "Actual" $ do
    withFile
      "inputs/day11.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day11 actualInput `shouldBe` ["10165598", "678728808158"]
      )