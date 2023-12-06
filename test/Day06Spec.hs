module Day06Spec (spec) where

import SpecHelper

testInput =
  "Time:      7  15   30\n\
  \Distance:  9  40  200"

spec :: Spec
spec = describe "Day 6" $ do
  it "Sample" $ do
    day06 testInput `shouldBe` ["288", "71503"]

  it "Actual" $ do
    withFile
      "inputs/day06.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day06 actualInput `shouldBe` ["1083852", "23501589"]
      )