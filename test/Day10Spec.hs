module Day10Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
    day10 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day10.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day10 actualInput `shouldBe` []
      )