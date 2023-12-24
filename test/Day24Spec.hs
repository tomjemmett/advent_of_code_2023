module Day24Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 24" $ do
  it "Actual" $ do
    withFile
      "inputs/day24.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day24 actualInput `shouldBe` ["12343", "769281292688187"]
      )