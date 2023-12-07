module Day07Spec (spec) where

import SpecHelper

testInput =
  "32T3K 765\n\
  \T55J5 684\n\
  \KK677 28\n\
  \KTJJT 220\n\
  \QQQJA 483"


spec :: Spec
spec = describe "Day 7" $ do
  it "Sample" $ do
    day07 testInput `shouldBe` ["6440", "5905"]

  it "Actual" $ do
    withFile
      "inputs/day07.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day07 actualInput `shouldBe` ["252656917", "253499763"]
      )