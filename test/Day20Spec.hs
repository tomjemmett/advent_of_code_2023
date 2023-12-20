module Day20Spec (spec) where

import SpecHelper
  ( IOMode (ReadMode),
    Spec,
    day20,
    describe,
    hGetContents,
    it,
    shouldBe,
    withFile,
  )

spec :: Spec
spec = describe "Day 20" $ do
  it "Actual" $ do
    withFile
      "inputs/day20.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day20 actualInput `shouldBe` ["839775244", "207787533680413"]
      )