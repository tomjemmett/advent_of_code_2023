{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Common where

import Control.Applicative (liftA2)
import Data.Array qualified as A
import Data.Char (digitToInt)
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Vector ((!), (!?))
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type AOCSolution = String -> [String]

type Point2d = (Int, Int)

type Point3d = (Int, Int, Int)

type Grid2d = V.Vector (V.Vector Int)

type Interval = (Int, Int)

countTrue :: (Foldable f) => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

isBetween :: (Ord a) => (a, a) -> a -> Bool
isBetween (l, r) v = v >= l && v <= r

linesRead :: (Read a) => String -> [a]
linesRead = map read . lines

linesWords :: String -> [[String]]
linesWords = map words . lines

numbersStringToInt :: (String -> [String]) -> String -> [Int]
numbersStringToInt split = map read . split

wordSeparatedInts :: String -> [Int]
wordSeparatedInts = numbersStringToInt words

commaSeparatedInts :: String -> [Int]
commaSeparatedInts = numbersStringToInt (splitOn ",")

bitStringToInt :: String -> Int
bitStringToInt = bitsToInt . map digitToInt

bitsToInt :: [Int] -> Int
bitsToInt = sum . zipWith (*) (map (2 ^) [0 ..]) . reverse

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

parseGrid2d :: String -> Grid2d
parseGrid2d = V.fromList . map V.fromList . map2 digitToInt . lines

lookupInGrid2d :: Grid2d -> Point2d -> Int
lookupInGrid2d i = fromMaybe 9 . lookupInGrid2d' i

lookupInGrid2d' :: Grid2d -> Point2d -> Maybe Int
lookupInGrid2d' i (r, c) = i V.!? r >>= flip (V.!?) c

point2dNeighbours :: Point2d -> [Point2d]
point2dNeighbours (r, c) = [(pred r, c), (succ r, c), (r, pred c), (r, succ c)]

point2dNeighboursDiags :: Point2d -> [Point2d]
point2dNeighboursDiags (r, c) =
  point2dNeighbours (r, c)
    ++ [ (pred r, pred c),
         (pred r, succ c),
         (succ r, pred c),
         (succ r, succ c)
       ]

point3dNeighbours :: Point3d -> [Point3d]
point3dNeighbours (x, y, z) =
  [ (pred x, y, z),
    (succ x, y, z),
    (x, pred y, z),
    (x, succ y, z),
    (x, y, pred z),
    (x, y, succ z)
  ]

sortPoint2d :: [Point2d] -> [Point2d]
sortPoint2d = sortBy comparePoint2d

comparePoint2d :: Point2d -> Point2d -> Ordering
comparePoint2d (a, b) (c, d)
  | a < c = LT
  | a > c = GT
  | b < d = LT
  | b > d = GT
  | otherwise = EQ

point3dX, point3dY, point3dZ :: Point3d -> Int
point3dX (x, _, _) = x
point3dY (_, y, _) = y
point3dZ (_, _, z) = z

median :: (Ord a) => [a] -> [a]
median x = if odd lx then [xs !! hl] else [xs !! pred hl, xs !! hl]
  where
    xs = sort x
    lx = length xs
    hl = lx `div` 2

tuplify2 :: [a] -> (a, a)
tuplify2 [a, b] = (a, b)

untuplify2 :: (a, a) -> [a]
untuplify2 (a, b) = [a, b]

tuplify3 :: [a] -> (a, a, a)
tuplify3 [a, b, c] = (a, b, c)

untuplify3 :: (a, a, a) -> [a]
untuplify3 (a, b, c) = [a, b, c]

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (flip compare)

manhattanDistance :: Point2d -> Point2d -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

hammingDistance :: (Eq b) => [b] -> [b] -> Int
hammingDistance xs ys = sum . map fromEnum $ zipWith (/=) xs ys

intervalsIntersect :: Interval -> Interval -> Bool
intervalsIntersect (a1, a2) (b1, b2) =
  or
    [ a1 <= b1 && a2 >= b1,
      a1 <= b1 && a2 >= b2,
      a1 >= b1 && a2 <= b2,
      a1 >= b1 && a2 >= b1
    ]

reduceIntervals :: [Interval] -> [Interval]
reduceIntervals [b] = [b]
reduceIntervals (a@(a1, a2) : b@(b1, b2) : xs)
  | intervalsIntersect a b = reduceIntervals ((min a1 b1, max a2 b2) : xs)
  | otherwise = a : reduceIntervals (b : xs)
  where
    i = intervalsIntersect a b

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

splitDoubleNewlines :: [Char] -> [[Char]]
splitDoubleNewlines = splitOn "\n\n"

countReduce :: (Foldable t, Ord a) => t a -> M.Map a Int
countReduce = foldr (flip (M.insertWith (+)) 1) M.empty

(<&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&>) = liftA2 (&&)

-- good example of DP memoization in Haskell
--  source: https://jelv.is/blog/Lazy-Dynamic-Programming/
wagnerFischer :: String -> String -> Int
wagnerFischer a b = d m n
  where
    (m, n) = (length a, length b)
    a' = A.listArray (1, m) a
    b' = A.listArray (1, n) b
    -- initialise the array: if we haven't consumed any values of b (j == 0)
    -- then the edit distance is the amount of values we have consumed of a (i)
    d i 0 = i
    -- and vice versa
    d 0 j = j
    -- now, compare each position in a to each position in b
    d i j
      -- the characters at i and j are the same, use the edit distance from the
      -- previous point
      | a' A.! i == b' A.! j = ds A.! (i - 1, j - 1)
      -- our values don't match, so find the smallest edit from the previous
      -- values in the array
      | otherwise =
          minimum
            [ ds A.! (i - 1, j) + 1, -- delete
              ds A.! (i, j - 1) + 1, -- insert
              ds A.! (i - 1, j - 1) + 1 -- modify
            ]
    -- build the array, using the d function from above
    ds = A.listArray bounds [d i j | (i, j) <- A.range bounds]
    bounds = ((0, 0), (m, n))

-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)

infixr 8 ...

-- Parsec
parse :: Parser a -> String -> a
parse = either (error . show) id ... flip P.parse ""

parseLines :: Parser a -> String -> [a]
parseLines p = parse (p `P.sepEndBy` P.newline)

parseLinesWith :: Parser a -> String -> [a]
parseLinesWith p s = parse p <$> lines s

number :: Parser Int
number = lexeme number'

number' :: Parser Int
number' =
  P.choice
    [ P.char '-' *> fmap negate digits,
      P.char '+' *> digits,
      digits
    ]
  where
    digits = read <$> P.many1 P.digit

numbers :: String -> Parser [Int]
numbers s = number `P.sepBy` (P.many1 . P.oneOf) s

lexeme :: P.Parsec String u a -> P.Parsec String u a
lexeme p = p <* P.spaces

symbol :: String -> Parser String
symbol = lexeme . P.try . P.string

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = P.between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = P.between (symbol "[") (symbol "]")

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`P.sepBy` symbol ",")

tryAll :: [Parser a] -> Parser a
tryAll = P.choice . map P.try

point2dGridToString :: Char -> (Point2d, Point2d) -> HM.HashMap Point2d Char -> String
point2dGridToString d ((minx, miny), (maxx, maxy)) g =
  unlines
    [ [HM.lookupDefault d (x, y) g | x <- [minx .. maxx]] | y <- [miny .. maxy]
    ]

point2dGridToString' :: Char -> HM.HashMap Point2d Char -> String
point2dGridToString' d g = point2dGridToString d (bounds $ HM.keys g) g

bounds :: [Point2d] -> (Point2d, Point2d)
bounds k = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs = map fst k
    ys = map snd k

--

findRepeatingPattern :: (Hashable k) => Int -> (b -> b) -> (b -> k) -> (b -> a) -> b -> a
findRepeatingPattern iterations f createKey solve input = values HM.! index
  where
    init = HM.singleton (createKey input) (0, solve input)
    ((end, start), values) = go f createKey solve 1 init input
    loop = end - start
    index = ((iterations - start) `mod` loop) + start + 1
    go ::
      (Hashable k) =>
      (b -> b) ->
      (b -> k) ->
      (b -> a) ->
      Int ->
      HM.HashMap k (Int, a) ->
      b ->
      ((Int, Int), HM.HashMap Int a)
    go f createKey solve n m i = case HM.lookup k m of
      Just r -> ((n, fst r), HM.fromList $ HM.elems m)
      Nothing -> go f createKey solve (succ n) m' i'
      where
        i' = f i
        k = createKey i'
        m' = HM.insert k (n, solve i) m

both :: (a, a) -> (a -> b) -> (b, b)
both (x, y) f = (f x, f y)