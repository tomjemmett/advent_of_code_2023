module Main where

import Days
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ runDay [1 .. 25]
    else runDay (read $ args !! 0)
