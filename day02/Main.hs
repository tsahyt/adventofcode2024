module Main where

import Data.List (sort, subsequences)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

part2 :: [[Int]] -> Int
part2 = length . filter isSafeDampened

isSafeDampened :: (Ord a, Num a) => [a] -> Bool
isSafeDampened xs = any isSafe subseq
  where
    n = length xs
    subseq = reverse . filter (\x -> length x >= n - 1) . subsequences $ xs

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe xs = distances && monotonic
  where
    distances = all (\(a, b) -> abs (b - a) > 0 && abs (b - a) <= 3) . pairs $ xs
    monotonic = sort xs == xs || sort xs == reverse xs

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> readFile "inputs/day02"
  print $ part1 input
  print $ part2 input
