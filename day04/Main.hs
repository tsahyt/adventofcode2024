module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

parse :: String -> M.Map (Int, Int) Char
parse = M.fromList .
  concatMap (\(row, s) -> zipWith (\col c -> ((row, col), c)) [0..] s) . zip [0..] . lines

gridlines :: Int -> Int -> [[(Int, Int)]]
gridlines m n = do
  i <- [0..m + 1]
  j <- [0..n + 1]
  let
    vertical = map (, j) [i .. i + 3]
    horizontal = map (i,) [j .. j + 3]
    diagonal1 = zip [i .. i + 3] [j .. j + 3]
    diagonal2 = zip [i .. i + 3] [j, j - 1 .. j - 3]
  [vertical, reverse vertical, horizontal, reverse horizontal,
   diagonal1, reverse diagonal1, diagonal2, reverse diagonal2]

crosses :: Int -> Int -> [([(Int, Int)], [(Int, Int)])]
crosses m n = do
  i <- [0..m + 1]
  j <- [0..n + 1]
  pure
    (
      [(i - 1, j - 1), (i, j), (i + 1, j + 1)],
      [(i - 1, j + 1), (i, j), (i + 1, j - 1)]
    )

readPuzzle :: M.Map (Int, Int) Char -> [(Int, Int)] -> String
readPuzzle p = map (\x -> fromMaybe '.' $ M.lookup x p)

extent :: M.Map (Int, Int) a -> (Int, Int)
extent x =
  let
    (rows, cols) = unzip . M.keys $ x
  in (maximum rows, maximum cols)

part1 :: M.Map (Int, Int) Char -> Int
part1 x = length $ filter ((== "XMAS") . readPuzzle x) (gridlines m n)
  where
    (m, n) = extent x

part2 :: M.Map (Int, Int) Char -> Int
part2 x = length $ filter (uncurry go) (crosses m n)
  where
    go p1 p2 = isMas p1 && isMas p2
    isMas ps = flip elem ["MAS", "SAM"] $ readPuzzle x ps
    (m, n) = extent x

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day04"
  print $ part1 input
  print $ part2 input
