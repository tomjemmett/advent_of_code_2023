module Day12Spec (spec) where

import SpecHelper

testInput =
  "???.### 1,1,3\n\
  \.??..??...?##. 1,1,3\n\
  \?#?#?#?#?#?#?#? 1,3,1,6\n\
  \????.#...#... 4,1,1\n\
  \????.######..#####. 1,6,5\n\
  \?###???????? 3,2,1"

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    day12 testInput `shouldBe` ["21", "525152"]

  it "Actual" $ do
    withFile
      "inputs/day12.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day12 actualInput `shouldBe` ["7716", "18716325559999"]
      )