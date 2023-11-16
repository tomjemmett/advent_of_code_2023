module Day05Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 5" $ do
  it "Sample" $ do
    day05 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day05.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day05 actualInput `shouldBe` []
      )