module Day10 where -- (day10) where

import Common
import PathFinding
import Control.Monad (ap, guard, forM_)
import Control.Monad.State
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Vector.Generic.Mutable (fill)

day10 :: AOCSolution
day10 = map show . ap [part1, part2] . pure . parseInput


findLoop :: M.HashMap Point2d Char -> S.HashSet Point2d
findLoop m = go S.empty s
  where
    s = M.keys $ M.filter (=='S') m
    go :: S.HashSet Point2d -> [Point2d] -> S.HashSet Point2d
    go s [] = s
    go s (p:ps) = if S.member p s
      then go s ps
      else go (S.insert p s) (ps ++ ns)
      where
        ns = neighbours m p
    neighbours :: M.HashMap Point2d Char -> Point2d -> [Point2d]
    neighbours m p@(x, y) = mapMaybe g $ case m M.! p of
      '-' ->
        [ ((x-1, y), "FL-")
        , ((x+1, y), "J7-")
        ]
      '|' ->
        [ ((x, y-1), "F7|")
        , ((x, y+1), "LJ|")
        ]
      'F' ->
        [ ((x+1, y), "J7-")
        , ((x, y+1), "LJ|")
        ]
      '7' ->
        [ ((x-1, y), "FL-")
        , ((x, y+1), "LJ|")
        ]
      'J' ->
        [ ((x-1, y), "FL-")
        , ((x, y-1), "F7|")
        ]
      'L' ->
        [ ((x+1, y), "J7-")
        , ((x, y-1), "F7|")
        ]
      'S' -> 
        [ ((x-1, y), "FL-")
        , ((x+1, y), "J7-")
        , ((x, y-1), "F7|")
        , ((x, y+1), "LJ|")
        ]
      '.' -> []
      where
        f = flip (M.findWithDefault '.') m
        g (p, t) = if f p `elem` t then Just p else Nothing

        
part1 :: M.HashMap Point2d Char -> Int
part1 = (`div` 2) . M.size . M.filter (/= '.')

part2 :: M.HashMap Point2d Char -> Int
part2 = M.size . M.filter (== '.') . reduceGrid . fillGrid . doubleGrid

doubleGrid :: M.HashMap Point2d Char -> M.HashMap Point2d Char
doubleGrid m = M.fromList $ concatMap f $ M.toList m
  where
    f (((*2) -> x, (*2) -> y), v) = ((x, y), v) : case v of
      '.' -> []
      '-' -> [((x + 1, y), '-')]
      '|' -> [((x, y + 1), '|')]
      'F' -> [((x + 1, y), '-'), ((x, y + 1), '|')]
      '7' -> [((x, y + 1), '|')]
      'L' -> [((x + 1, y), '-')]
      'J' -> []
      'S' -> [ ((x + 1, y), '-')
             | M.findWithDefault '.' ((x `div` 2) + 1, y `div` 2) m `elem` "J7-"] ++
             [ ((x, y + 1), '|')
             | M.findWithDefault '.' (x `div` 2, (y `div` 2) + 1) m `elem` "JL|"]

reduceGrid :: M.HashMap Point2d Char -> M.HashMap Point2d Char
reduceGrid = M.fromList . mapMaybe g . M.toList
  where
    g :: (Point2d, Char) -> Maybe (Point2d, Char)
    g ((x, y), v)
      | odd x = Nothing
      | odd y = Nothing
      | otherwise = Just ((x `div` 2, y `div` 2), v)

fillGrid :: M.HashMap Point2d Char -> M.HashMap Point2d Char
fillGrid m = go m op
  where
    lu = flip (M.findWithDefault '.') m
    op = filter ((=='.') . lu) $ outerPoints $ bounds $ M.keys m
    go :: M.HashMap Point2d Char -> [Point2d] -> M.HashMap Point2d Char
    go m [] = m
    go m (p:ps) = if M.lookupDefault '.' p m /= '.'
      then go m ps
      else go m' (n ++ ps)
      where
        n = filter (inBounds m) $ point2dNeighbours p
        m' = M.insert p 'O' m

outerPoints :: (Point2d, Point2d) -> [Point2d]
outerPoints ((minx, miny), (maxx, maxy)) =
  [ (x, y)
  | x <- [minx..maxx]
  , y <- [miny, maxy]
  ] ++ 
  [ (x, y)
  | x <- [minx, maxx]
  , y <- [miny..maxy]
  ]

inBounds :: M.HashMap Point2d a -> Point2d -> Bool
inBounds m (x, y) = x >= minx && x <= maxx && y >= miny && y <= maxy
  where
    ((minx, miny), (maxx, maxy)) = bounds $ M.keys m

parseInput :: String -> M.HashMap Point2d Char
parseInput input = M.mapWithKey (\k v -> if S.member k loop then v else '.') i
  where
    i = M.fromList
      [ ((c, r), v)
      | (r, line) <- zip [0..] $ lines input
      , (c, v) <- zip [0..] line]  
    loop = findLoop i
