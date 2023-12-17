module Day17Spec (spec) where

import SpecHelper

testInput =
  "2413432311323\n\
  \3215453535623\n\
  \3255245654254\n\
  \3446585845452\n\
  \4546657867536\n\
  \1438598798454\n\
  \4457876987766\n\
  \3637877979653\n\
  \4654967986887\n\
  \4564679986453\n\
  \1224686865563\n\
  \2546548887735\n\
  \4322674655533"

spec :: Spec
spec = describe "Day 17" $ do
  it "Sample" $ do
    day17 testInput `shouldBe` ["102", "94"]

  it "Actual" $ do
    withFile
      "inputs/day17.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day17 actualInput `shouldBe` ["956", "1106"]
      )