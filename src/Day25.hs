module Day25 (day25) where

import Common
import Control.Monad (ap)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Graph.Inductive qualified as G
import Data.List (delete, lookup, maximumBy, nub, (\\))
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.Parsec qualified as P

{-
# R Solution
library(tidyverse)
library(igraph)
library(tidygraph)

input <- read_file("inputs/day25.txt") |>
  stringr::str_trim() |>
  str_split("\n") |>
  pluck(1)

g <- input |>
  str_split(": ") |>
  map(\(.x) {
    tibble(from = .x[[1]], to = stringr::str_split(.x[[2]], " ")[[1]])
  }) |>
  bind_rows() |>
  graph_from_data_frame(directed = FALSE)

cut <- min_cut(g, value.only = FALSE)

length(cut$partition1) * length(cut$partition2)
-}

day25 :: AOCSolution
day25 = map show . ap [solve] . pure . parseInput

mkGraph :: ([String], [(String, String)]) -> G.Gr String String
mkGraph (n, e) = G.undir $ G.mkGraph (map swap n') e'
  where
    n' = zip n [1 ..]
    e' = map (\(from, to) -> (fromJust $ lookup from n', fromJust $ lookup to n', from ++ " " ++ to)) e

solve :: G.Gr String String -> Int
solve g = group1size * (G.order g - group1size)
  where
    group1 = loop (G.nodes g)
    group1size = length group1
    loop s = if sum (map count s) == 3 then s else loop (delete (maximumBy (compare `on` count) s) s)
      where
        count v = length (G.suc g v \\ s)

parseInput :: String -> G.Gr String String
parseInput =
  mkGraph
    . bimap (nub . concat) concat
    . unzip
    . parseLinesWith do
      f <- P.manyTill P.letter (symbol ":")
      t <- P.many P.letter `P.sepBy` P.char ' '
      pure (f : t, map (f,) t)
