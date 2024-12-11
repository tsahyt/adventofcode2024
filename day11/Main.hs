module Main where

import Data.MemoTrie

stones :: Int -> Int -> Int
stones = memo2 go
  where
    go :: Int -> Int -> Int
    go 0 _ = 1
    go n x
      | even $ length (show x) =
        let
          (l, r) = splitAt (length (show x) `div` 2) (show x)
        in stones (n - 1) (read l) + stones (n - 1) (read r)
      | x == 0 = stones (n - 1) 1
      | otherwise = stones (n - 1) (x * 2024)

part1 :: [Int] -> Int
part1 = sum . map (stones 25)

part2 :: [Int] -> Int
part2 = sum . map (stones 75)

main :: IO ()
main = do
  input <- map (read @Int) . words <$> readFile "inputs/day11"
  print $ part1 input
  print $ part2 input
