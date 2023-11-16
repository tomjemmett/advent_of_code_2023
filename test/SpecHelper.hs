module SpecHelper (
  module Test.Hspec,
  module Days,
  module System.IO
) where

import Test.Hspec
import Days
import System.IO ( hGetContents, withFile, IOMode(ReadMode) )