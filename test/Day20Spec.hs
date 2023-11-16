module Day20Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 20" $ do
  it "Sample" $ do
    day20 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day20.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day20 actualInput `shouldBe` []
      )