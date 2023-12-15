module Day15Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 15" $ do
  it "Sample" $ do
    day15 testInput `shouldBe` ["1320", "145"]

  it "Actual" $ do
    withFile
      "inputs/day15.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day15 actualInput `shouldBe` ["511215", "236057"]
      )