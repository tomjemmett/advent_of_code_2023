module Day08Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 8" $ do
  it "Sample" $ do
    day08 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day08.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day08 actualInput `shouldBe` []
      )