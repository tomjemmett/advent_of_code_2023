module Day22Spec (spec) where

import SpecHelper

testInput =
  "1,0,1~1,2,1\n\
  \0,0,2~2,0,2\n\
  \0,2,3~2,2,3\n\
  \0,0,4~0,2,4\n\
  \2,0,5~2,2,5\n\
  \0,1,6~2,1,6\n\
  \1,1,8~1,1,9"

spec :: Spec
spec = describe "Day 22" $ do
  it "Sample" $ do
    day22 testInput `shouldBe` ["5", "7"]

  it "Actual" $ do
    withFile
      "inputs/day22.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day22 actualInput `shouldBe` ["485", "74594"]
      )