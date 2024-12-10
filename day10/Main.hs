module Main where

import qualified Data.Map as M
import Data.Char
import Data.List

parse :: String -> M.Map (Int, Int) Int
parse =
  M.fromList .
  concatMap (\(i, x) -> zipWith (\j c -> ((i, j), digitToInt c)) [0..] x) .
  zip [0..] . lines

dfs :: (a -> [a]) -> (a -> Bool) -> a -> [a]
dfs suc goal current
  | goal current = pure current
  | otherwise = concat [ dfs suc goal x | x <- suc current ]

uphill :: M.Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
uphill m x@(i, j)
  | Just h <- M.lookup x m =
      filter (\n -> Just (h + 1) == M.lookup n m)
      [(i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1)]
  | otherwise = []

solve :: ([(Int, Int)] -> [(Int, Int)]) -> M.Map (Int, Int) Int -> Int
solve f input = sum $
  map
  (length . f . dfs (uphill input) (\z -> M.lookup z input == Just 9))
  (M.keys (M.filter (== 0) input))

part1 :: M.Map (Int, Int) Int -> Int
part1 = solve nub

part2 :: M.Map (Int, Int) Int -> Int
part2 = solve id

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day10"
  print $ part1 input
  print $ part2 input
