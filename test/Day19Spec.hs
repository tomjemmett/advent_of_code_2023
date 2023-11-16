module Day19Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 19" $ do
  it "Sample" $ do
    day19 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day19.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day19 actualInput `shouldBe` []
      )