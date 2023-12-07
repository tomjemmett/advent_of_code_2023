module Day07 where --(day07) where

import Common
import Control.Monad (ap)
import Data.Function (on)
import Data.List (sortBy, group, elemIndex, splitAt)
import Text.Parsec qualified as P

type Input = [((Hand, [Card]), Int)]
data Hand
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind deriving (Show, Eq, Ord)
data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace deriving (Show, Eq, Ord)

day07 :: AOCSolution
day07 = map (show . solve) . ap (parseInput <$> [False, True]) . pure

solve :: Input -> Int
solve = sum . zipWith (*) [1..] . map snd

parseInput :: Bool -> String -> Input
parseInput joker = sortBy (compare `on` fst) . parseLinesWith do
  cs <- parseCards joker <$> P.many (P.oneOf "23456789TJQKA")
  bid <- P.char ' ' >> number
  pure ((handType cs, cs), bid)

parseCards :: Bool -> String -> [Card]
parseCards joker = map parseCard
  where
    parseCard = \case
      '2' -> Two
      '3' -> Three
      '4' -> Four
      '5' -> Five
      '6' -> Six
      '7' -> Seven
      '8' -> Eight
      '9' -> Nine
      'T' -> Ten
      'J' -> if joker then Joker else Jack
      'Q' -> Queen
      'K' -> King
      'A' -> Ace 

handType :: [Card] -> Hand
handType xs = case length xs' of
  1 -> FiveOfAKind
  2 -> if fst (head xs') == 4 
    then FourOfAKind
    else FullHouse
  3 -> if fst (head xs') == 3
    then ThreeOfAKind
    else TwoPair
  4 -> OnePair
  5 -> HighCard
  where
    xs' = replaceJokers . countCards $ xs
    countCards :: [Card] -> [(Int, Card)]
    countCards = sortBy (flip compare `on` fst) .
          map (\xs@(x:_) -> (length xs, x)) .
          group .
          sortBy (flip compare)
    replaceJokers :: [(Int, Card)] -> [(Int, Card)]
    replaceJokers xs 
      | length xs == 1 = xs
      | otherwise = case Joker `elemIndex` map snd xs of
        Nothing -> xs
        Just i ->
          let
            (a, (j, _):b) = splitAt i xs
            ((c, d):e) = a ++ b
          in 
            (c+j, d):e