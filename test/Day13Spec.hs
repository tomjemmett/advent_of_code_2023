module Day13Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
    day13 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day13.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day13 actualInput `shouldBe` []
      )