module Day08Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 8" $ do
  it "Actual" $ do
    withFile
      "inputs/day08.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day08 actualInput `shouldBe` ["22199", "13334102464297"]
      )