module Day22 (day22) where

import Common
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (nub, sortBy)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Parsec qualified as P

type Brick = [Point3d]

type Input = [(Int, Brick)]

day22 :: AOCSolution
day22 input = map show [p1, p2]
  where
    i = parseInput input
    g = dropBricks i
    r = restingOn g
    r' = M.map (\x -> if S.null x then S.singleton (-1) else x) r
    d = cannotDisintegrate r
    p1 = M.size r - S.size d
    p2 = sum $ map (newfall r' S.empty . S.singleton) $ S.toList d

newfall :: M.HashMap Int (S.HashSet Int) -> S.HashSet Int -> S.HashSet Int -> Int
newfall r f nf
  | S.size nf <= S.size f = S.size f - 1
  | otherwise = newfall r f' nf'
  where
    f' = S.union f nf
    nf' = S.union nf $ S.fromList $ M.keys $ M.filter (all (`S.member` nf) . S.toList) r

cannotDisintegrate :: M.HashMap Int (S.HashSet Int) -> S.HashSet Int
cannotDisintegrate r = S.unions $ M.elems $ M.filter ((<= 1) . S.size) r

restingOn :: M.HashMap Point3d Int -> M.HashMap Int (S.HashSet Int)
restingOn g = foldr (uncurry $ M.insertWith S.union) M.empty $ mapMaybe f ks
  where
    ks = sortBy (compare `on` point3dZ) $ M.keys g
    f k@(x, y, z) = case M.lookup (x, y, pred z) g of
      Nothing -> Just (i, S.empty)
      Just j -> if i == j then Nothing else Just (i, S.singleton j)
      where
        i = g M.! k

dropBricks :: [(Int, Brick)] -> M.HashMap Point3d Int
dropBricks = foldl dropBrick M.empty

dropBrick :: M.HashMap Point3d Int -> (Int, Brick) -> M.HashMap Point3d Int
dropBrick g (i, bs)
  | any (`M.member` g) bs' = g'
  | any ((== 0) . point3dZ) bs' = g'
  | otherwise = dropBrick g (i, bs')
  where
    bs' = map drop bs
    drop (x, y, z) = (x, y, pred z)
    g' = foldr (`M.insert` i) g bs

parseInput :: String -> Input
parseInput =
  zip [0 ..]
    . map fst
    . sortBy (compare `on` snd)
    . parseLinesWith do
      [x1, y1, z1] <- number `P.sepBy` P.char ','
      P.char '~'
      [x2, y2, z2] <- number `P.sepBy` P.char ','
      pure ([(x, y, z) | x <- [x1 .. x2], y <- [y1 .. y2], z <- [z1 .. z2]], min z1 z2)
