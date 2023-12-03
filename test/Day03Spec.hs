module Day03Spec (spec) where

import SpecHelper

testInput =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598.."

spec :: Spec
spec = describe "Day 3" $ do
  it "Sample" $ do
    day03 testInput `shouldBe` ["4361", "467835"]

  it "Actual" $ do
    withFile
      "inputs/day03.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day03 actualInput `shouldBe` ["498559", "72246648"]
      )