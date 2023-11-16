module Day22Spec (spec) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 22" $ do
  it "Sample" $ do
    day22 testInput `shouldBe` []

  it "Actual" $ do
    withFile
      "inputs/day22.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day22 actualInput `shouldBe` []
      )