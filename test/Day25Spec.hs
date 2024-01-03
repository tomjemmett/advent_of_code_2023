module Day25Spec (spec) where

import SpecHelper

testInput =
  "jqt: rhn xhk nvd\n\
  \rsh: frs pzl lsr\n\
  \xhk: hfx\n\
  \cmg: qnr nvd lhk bvb\n\
  \rhn: xhk bvb hfx\n\
  \bvb: xhk hfx\n\
  \pzl: lsr hfx nvd\n\
  \qnr: nvd\n\
  \ntq: jqt hfx bvb xhk\n\
  \nvd: lhk\n\
  \lsr: lhk\n\
  \rzs: qnr cmg lsr rsh\n\
  \frs: qnr lhk lsr"

spec :: Spec
spec = describe "Day 25" $ do
  it "Sample" $ do
    day25 testInput `shouldBe` ["54"]

  it "Actual" $ do
    withFile
      "inputs/day25.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day25 actualInput `shouldBe` ["525264"]
      )