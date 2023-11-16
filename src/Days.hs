module Days
  ( day01,
    day02,
    day03,
    day04,
    day05,
    day06,
    day07,
    day08,
    day09,
    day10,
    day11,
    day12,
    day13,
    day14,
    day15,
    day16,
    day17,
    day18,
    day19,
    day20,
    day21,
    day22,
    day23,
    day24,
    day25,
    runDay,
  )
where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import System.Directory (doesFileExist)

days =
  [ (day01, "inputs/day01.txt"),
    (day02, "inputs/day02.txt"),
    (day03, "inputs/day03.txt"),
    (day04, "inputs/day04.txt"),
    (day05, "inputs/day05.txt"),
    (day06, "inputs/day06.txt"),
    (day07, "inputs/day07.txt"),
    (day08, "inputs/day08.txt"),
    (day09, "inputs/day09.txt"),
    (day10, "inputs/day10.txt"),
    (day11, "inputs/day11.txt"),
    (day12, "inputs/day12.txt"),
    (day13, "inputs/day13.txt"),
    (day14, "inputs/day14.txt"),
    (day15, "inputs/day15.txt"),
    (day16, "inputs/day16.txt"),
    (day17, "inputs/day17.txt"),
    (day18, "inputs/day18.txt"),
    (day19, "inputs/day19.txt"),
    (day20, "inputs/day20.txt"),
    (day21, "inputs/day21.txt"),
    (day22, "inputs/day22.txt"),
    (day23, "inputs/day23.txt"),
    (day24, "inputs/day24.txt"),
    (day25, "inputs/day25.txt")
  ]

runDay :: Int -> IO ()
runDay day = do
  let (fn, file) = days !! pred day
  fileExists <- doesFileExist file
  if fileExists
    then do
      putStrLn $ replicate 80 '-'
      putStr $ "Day: " ++ show day
      putStrLn ""
      input <- readFile file
      putStr $ unlines $ fn input
    else do
      putStr ""