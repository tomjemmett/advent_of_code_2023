module Day15Spec (spec) where

import SpecHelper

testInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

spec :: Spec
spec = describe "Day 15" $ do
  it "Sample" $ do
    day15 testInput `shouldBe` ["1320", "145"]

  it "Actual" $ do
    withFile
      "inputs/day15.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day15 actualInput `shouldBe` ["511215", "236057"]
      )