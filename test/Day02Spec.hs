module Day02Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 2" $ do
  it "Sample" $ do
    day02 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day02.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day02 actualInput `shouldBe` []
      )