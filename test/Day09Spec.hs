module Day09Spec (spec) where

import SpecHelper

testInput =
  "0 3 6 9 12 15\n\
  \1 3 6 10 15 21\n\
  \10 13 16 21 30 45"

spec :: Spec
spec = describe "Day 9" $ do
  it "Sample" $ do
    day09 testInput `shouldBe` ["114", "2"]

  it "Actual" $ do
    withFile
      "inputs/day09.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day09 actualInput `shouldBe` ["2005352194", "1077"]
      )