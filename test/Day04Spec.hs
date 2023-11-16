module Day04Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 4" $ do
  it "Sample" $ do
    day04 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day04.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day04 actualInput `shouldBe` []
      )