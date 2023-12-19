module Day19 (day19) where

import Common
import Control.Monad (ap)
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

type Workflow = (Char, Char, Int, String)

type Workflows = M.HashMap String [Workflow]

type Part = M.HashMap Char Int

day19 :: AOCSolution
day19 = map show . ap [part1, part2] . pure . parseInput

part1 :: (Workflows, [Part]) -> Int
part1 (workflows, parts) = sum $ map (sum . map snd . M.toList) $ f parts
  where
    f = filter (checkPart "in" workflows)
    checkPart :: String -> Workflows -> Part -> Bool
    checkPart w workflows part = applyRules (workflows M.! w)
      where
        applyRules :: [Workflow] -> Bool
        applyRules (rule : rs) = case r rule part of
          Just to -> f to
          Nothing -> applyRules rs
          where
            f = \case
              "A" -> True
              "R" -> False
              to -> checkPart to workflows part
            r ('_', '_', 0, to) _ = Just to
            r (category, \c -> if c == '>' then (>) else (<) -> comp, value, to) m = if (m M.! category) `comp` value then Just to else Nothing

part2 :: (Workflows, [Part]) -> Int
part2 (workflows, _) = g initialParts "in"
  where
    g parts = \case
      "A" -> product $ map (\(_, (l, h)) -> h - l + 1) $ M.toList parts
      "R" -> 0
      to -> f (workflows M.! to) parts
    f :: [Workflow] -> M.HashMap Char (Int, Int) -> Int
    f [] _ = 0
    f (('_', '_', 0, to) : _) parts = g parts to
    f ((category, '>', value, to) : ws) parts
      | h < value = f ws parts
      | l >= value = g parts to
      | otherwise = f ws partsL + g partsH to
      where
        (l, h) = parts M.! category
        partsH = M.insert category (value + 1, h) parts
        partsL = M.insert category (l, value) parts
    f ((category, '<', value, to) : ws) parts
      | l >= value = f ws parts
      | h < value = g parts to
      | otherwise = f ws partsH + g partsL to
      where
        (l, h) = parts M.! category
        partsH = M.insert category (value, h) parts
        partsL = M.insert category (l, value - 1) parts

    initialParts :: M.HashMap Char (Int, Int)
    initialParts = M.fromList [('x', (1, 4000)), ('m', (1, 4000)), ('a', (1, 4000)), ('s', (1, 4000))]

parseInput :: String -> (Workflows, [Part])
parseInput = parse do
  workflows <- pWorkflow `P.sepEndBy` P.newline
  P.newline
  parts <- pPart `P.sepEndBy` P.newline
  pure (M.fromList workflows, parts)
  where
    pWorkflow = do
      name <- P.many1 P.letter
      rules <- (P.char '{' `P.between` P.char '}') ((P.try pRule P.<|> pOtherwise) `P.sepBy` P.char ',')
      pure (name, rules)
    pRule = do
      category <- P.letter
      comp <- P.oneOf "<>"
      value <- number
      to <- P.char ':' *> P.many1 P.letter
      pure (category, comp, value, to)
    pOtherwise = do
      to <- P.many1 P.letter
      pure ('_', '_', 0, to)
    --
    pPart =
      M.fromList <$> (P.char '{' `P.between` P.char '}') do
        flip P.sepBy (P.char ',') do
          a <- P.letter <* P.char '='
          b <- number
          pure (a, b)
