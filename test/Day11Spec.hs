module Day11Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
    day11 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day11.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day11 actualInput `shouldBe` []
      )