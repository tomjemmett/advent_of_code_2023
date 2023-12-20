module Day20 (day20) where

import Common
import Control.Monad (ap)
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Text.Parsec qualified as P

type Input = M.HashMap String (Module, [String])

data Module = Broadcaster | FlipFlop Bool | Conjunction (M.HashMap String Bool) deriving (Show)

day20 :: AOCSolution
day20 = map show . ap [part1, part2] . pure . parseInput

part1 :: Input -> Int
part1 i = ls * hs
  where
    r = iterate pressButton ((0, 0), i)
    (ls, hs) = fst $ r !! 1000

part2 :: Input -> Int
part2 i = find 1 1 ks ((0, 0), i')
  where
    ((Conjunction cj, _) : _) = M.elems $ M.filter ((== ["rx"]) . snd) i
    ks = M.keys cj
    i' = foldl (\m k -> M.insert k (FlipFlop False, []) m) i ks
    find :: Int -> Int -> [String] -> ((Int, Int), Input) -> Int
    find p _ [] _ = p
    find p n ks st = isSolved ks []
      where
        st'@(_, i'') = pressButton st
        isSolved [] ys = find p (succ n) ys st'
        isSolved (x : xs) ys =
          if v
            then find (p * n) (succ n) (ys ++ xs) st'
            else isSolved xs (x : ys)
          where
            (FlipFlop v, _) = i'' M.! x

pressButton :: ((Int, Int), Input) -> ((Int, Int), Input)
pressButton (s, i) = runState (go [("broadcaster", False, "")] s) i

go :: [(String, Bool, String)] -> (Int, Int) -> State Input (Int, Int)
go [] (ls, hs) = pure (ls, hs)
go ((curr, signal, input) : queue) (ls, hs) = do
  s <- get
  let m = M.lookup curr s
  q <- g m
  go (queue ++ q) (ls + 1 - fromEnum signal, hs + fromEnum signal)
  where
    g Nothing = pure []
    g (Just (t, to)) = f t to
    f :: Module -> [String] -> State Input [(String, Bool, String)]
    -- handle broadcaster
    f Broadcaster to = pure $ map (,signal,curr) to
    -- handle flip flops
    f (FlipFlop state) to =
      if signal
        then pure []
        else do
          modify $ M.insert curr (FlipFlop $ not state, to)
          pure $ map (,not state,curr) to
    -- handle conjunctions
    f (Conjunction state) to = do
      let state' = M.insert input signal state
          signal' = not $ and $ M.elems state'
      modify $ M.insert curr (Conjunction state', to)
      pure $ map (,signal',curr) to

isConjunction :: Module -> Bool
isConjunction (Conjunction _) = True
isConjunction _ = False

parseInput :: String -> Input
parseInput =
  initializeConjunctionModules . M.fromList . parseLines do
    (n, t) <- P.choice $ map P.try [pBroadcaster, pFlipFlop, pConjunction]
    symbol "->"
    to <- P.many1 P.letter `P.sepBy` symbol ","
    pure (n, (t, to))
  where
    initializeConjunctionModules i = foldl f i cm
      where
        cm = M.keys $ M.filter (isConjunction . fst) i
        f i x = M.insert x (Conjunction $ M.fromList $ map (,False) $ M.keys $ M.filter (elem x . snd) i, snd $ i M.! x) i
    --
    pBroadcaster = ("broadcaster", Broadcaster) <$ symbol "broadcaster"
    --
    pFlipFlop = do
      n <- P.char '%' *> P.many1 P.letter <* P.char ' '
      pure (n, FlipFlop False)
    --
    pConjunction = do
      n <- P.char '&' *> P.many1 P.letter <* P.char ' '
      pure (n, Conjunction M.empty)