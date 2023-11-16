module Day25Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 25" $ do
  it "Sample" $ do
    day25 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day25.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day25 actualInput `shouldBe` []
      )