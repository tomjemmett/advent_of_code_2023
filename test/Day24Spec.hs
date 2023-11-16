module Day24Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 24" $ do
  it "Sample" $ do
    day24 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day24.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day24 actualInput `shouldBe` []
      )