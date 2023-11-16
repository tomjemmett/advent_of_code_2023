module Day03Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 3" $ do
  it "Sample" $ do
    day03 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day03.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day03 actualInput `shouldBe` []
      )