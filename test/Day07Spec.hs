module Day07Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 7" $ do
  it "Sample" $ do
    day07 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day07.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day07 actualInput `shouldBe` []
      )