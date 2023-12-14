module Day14Spec (spec) where

import SpecHelper

testInput =
  "O....#....\n\
  \O.OO#....#\n\
  \.....##...\n\
  \OO.#O....O\n\
  \.O.....O#.\n\
  \O.#..O.#.#\n\
  \..O..#O..O\n\
  \.......O..\n\
  \#....###..\n\
  \#OO..#...."

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    day14 testInput `shouldBe` ["136", "64"]

  it "Actual" $ do
    withFile
      "inputs/day14.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day14 actualInput `shouldBe` ["102497", "105008"]
      )