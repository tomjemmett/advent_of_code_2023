module Day01Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 1" $ do
  it "Sample" $ do
    day01 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day01.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day01 actualInput `shouldBe` []
      )