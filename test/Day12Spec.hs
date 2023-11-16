module Day12Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    day12 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day12.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day12 actualInput `shouldBe` []
      )