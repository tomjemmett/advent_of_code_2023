module Day23 (day23) where

import Common
import Control.Monad (ap, guard)
import Control.Monad.Writer
import Data.Array qualified as A
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (elemIndex)
import Data.Semigroup (Max (..))

type Grid = A.Array Point2d Char

type Vertices = S.HashSet Point2d

type Edges = M.HashMap Point2d [(Point2d, Int)]

day23 :: AOCSolution
day23 input = show . f <$> [p1, p2]
  where
    (i, vs, start, end) = parseInput input
    p1 = makeEdges False i vs
    p2 = makeEdges True i vs
    f e = getMax $ execWriter $ dfs end e [((start, 0), S.empty)]

dfs :: Point2d -> Edges -> [((Point2d, Int), S.HashSet Point2d)] -> Writer (Max Int) ()
dfs _ _ [] = pure ()
dfs goal edges (((p, d), s) : q)
  | p `S.member` s = dfs goal edges q
  | p == goal = tell (Max d) >> dfs goal edges q
  | otherwise = do
      let es = edges M.! p
          s' = S.insert p s
          q' = map ((,s') . second (+ d)) es ++ q
      dfs goal edges q'

makeEdges :: Bool -> Grid -> Vertices -> Edges
makeEdges isPart2 i vs = M.fromList $ map (\v -> (v, f v [(v, 0)] S.empty [])) $ S.toList vs
  where
    inBounds = A.inRange (A.bounds i)
    f :: Point2d -> [(Point2d, Int)] -> S.HashSet Point2d -> [(Point2d, Int)] -> [(Point2d, Int)]
    f _ [] _ e = e
    f v ((p, d) : q) s e
      | p `S.member` s = f v q s e
      | p `S.member` vs && p /= v = f v q s' e'
      | otherwise = f v q' s' e
      where
        s' = S.insert p s
        e' = (p, d) : e
        ns = zip "<>^v" $ point2dNeighbours p
        q' =
          q ++ do
            (nc, np) <- ns
            guard $ inBounds np
            guard $ i A.! np /= '#'
            guard $ isPart2 || i A.! np `elem` '.' : [nc]
            pure (np, succ d)

parseInput :: String -> (Grid, Vertices, Point2d, Point2d)
parseInput input = (a, vs, start, end)
  where
    i = lines input
    nr = length i
    nc = length $ head i
    bounds = ((0, 0), (nc - 1, nr - 1))
    a = A.array ((0, 0), (nc - 1, nr - 1)) [((c, r), v) | (r, vs) <- zip [0 ..] i, (c, v) <- zip [0 ..] vs]
    Just start = (,0) <$> elemIndex '.' (head i)
    Just end = (,pred $ length i) <$> elemIndex '.' (last i)
    inBounds = A.inRange bounds
    vs =
      S.fromList $
        start : end : do
          (p, v) <- A.assocs a
          guard $ v /= '#'
          let ns = filter inBounds $ point2dNeighbours p
          guard $ countTrue (\p' -> a A.! p' /= '#') ns > 2
          pure p
