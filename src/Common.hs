{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Common where

import Control.Applicative (liftA2)
import Data.Char (digitToInt)
import Data.Foldable (toList)
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

countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

isBetween :: Ord a => (a, a) -> a -> Bool
isBetween (l, r) v = v >= l && v <= r

linesRead :: Read a => String -> [a]
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

parse :: Parser a -> String -> Either P.ParseError a
parse = flip P.parse ""

-- assumes that the parser always succeeds, it applies a function f to the Right value from the parser
parse' :: Parser a -> (a -> b) -> String -> b
parse' p f s = case parse p s of Right x -> f x

parseLines :: Parser a -> String -> [a]
parseLines p = parse' (p `P.sepEndBy` P.newline) id

number :: Parser Int
number =
  P.choice
    [ P.char '-' *> fmap negate digits,
      P.char '+' *> digits,
      digits
    ]
  where
    digits = read <$> P.many1 P.digit

numbers :: String -> Parser [Int]
numbers s = number `P.sepBy` (P.many1 . P.oneOf) s

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

median :: Ord a => [a] -> [a]
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

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

manhattanDistance :: Point2d -> Point2d -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

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