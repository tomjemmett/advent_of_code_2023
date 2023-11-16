module Day06Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 6" $ do
  it "Sample" $ do
    day06 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day06.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day06 actualInput `shouldBe` []
      )