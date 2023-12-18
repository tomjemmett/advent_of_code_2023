module Day18Spec (spec) where

import SpecHelper

testInput =
  "R 6 (#70c710)\n\
  \D 5 (#0dc571)\n\
  \L 2 (#5713f0)\n\
  \D 2 (#d2c081)\n\
  \R 2 (#59c680)\n\
  \D 2 (#411b91)\n\
  \L 5 (#8ceee2)\n\
  \U 2 (#caa173)\n\
  \L 1 (#1b58a2)\n\
  \U 2 (#caa171)\n\
  \R 2 (#7807d2)\n\
  \U 3 (#a77fa3)\n\
  \L 2 (#015232)\n\
  \U 2 (#7a21e3)"

spec :: Spec
spec = describe "Day 18" $ do
  it "Sample" $ do
    day18 testInput `shouldBe` ["62", "952408144115"]

  it "Actual" $ do
    withFile
      "inputs/day18.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day18 actualInput `shouldBe` ["38188", "93325849869340"]
      )