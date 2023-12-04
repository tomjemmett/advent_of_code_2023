module Day04 (day04) where

import Common
import Control.Monad (ap)
import Data.HashSet qualified as S
import Text.Parsec qualified as P

day04 :: AOCSolution
day04 = map show . go . parseInput

-- our fold function solves part 1 and 2 together. The accumulator that we use
-- is a list containing the answers for p1/2 respectively, and a list which
-- represents how many times each card should be represented (for part 2).

-- for part 1, if the amount of winning numbers for a card is zero, do nothing
-- otherwise take the winning numbers and 2 to the power of the predecessor of
-- that value. e.g. 1 -> 2^0 = 1, 2 -> 2^1 = 2, 3 -> 2^2 = 4, 4 -> 2^3 = 8, ...

-- for part 2, we update the accumulator with the amount of times that card is
-- represented (c), and then update the future list of counts by repeating the
-- count for this card x times (the amount that card won)

go :: [Int] -> [Int]
go = fst . foldl f ([0, 0], repeat 1)
  where
    f ([p1, p2], c : cs) x =
      ( [p1 + if x > 0 then 2 ^ (x - 1) else 0, p2 + c],
        zipWith (+) cs (replicate x c ++ repeat 0)
      )

-- parse each card into a number representing the amount of myNumbers which
-- matched winningNumbers, i.e. the amount that card won
parseInput :: String -> [Int]
parseInput = parse' (p `P.sepEndBy` P.newline) id
  where
    p = do
      P.string "Card " `P.between` P.char ':' $ q
      winningNumbers <- S.fromList <$> P.manyTill (q <* P.char ' ') (P.char '|')
      myNumbers <- S.fromList <$> q `P.sepBy` P.char ' '
      pure $ S.size $ S.intersection winningNumbers myNumbers
    q = P.many P.space *> number
