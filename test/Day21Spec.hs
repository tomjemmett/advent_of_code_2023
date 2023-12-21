module Day21Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 21" $ do
  it "Actual" $ do
    withFile
      "inputs/day21.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day21 actualInput `shouldBe` ["3697", "608152828731262"]
      )