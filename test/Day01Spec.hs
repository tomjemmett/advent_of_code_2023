module Day01Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 1" $ do

  it "Actual" $ do
    withFile
      "inputs/day01.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day01 actualInput `shouldBe` ["54990", "54473"]
      )