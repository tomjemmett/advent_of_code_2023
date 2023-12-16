module Day16Spec (spec) where

import SpecHelper

testInput =
  ".|...\\....\n\
  \|.-.\\.....\n\
  \.....|-...\n\
  \........|.\n\
  \..........\n\
  \..........\n\
  \..../.\\\\..\n\
  \.-.-/..|..\n\
  \.|....-|.\\\n\
  \..//.|...."

spec :: Spec
spec = describe "Day 6" $ do
  it "Sample" $ do
    day16 testInput `shouldBe` ["46", "51"]

  it "Actual" $ do
    withFile
      "inputs/day16.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day16 actualInput `shouldBe` ["7482", "7896"]
      )