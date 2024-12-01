module Main where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as IM

part1 :: [Int] -> [Int] -> Int
part1 as bs = sum $ zipWith (\a b -> abs $ a - b) (sort as) (sort bs)

part2 :: [Int] -> [Int] -> Int
part2 as bs =
  let
    factors = IM.fromList . map (\xs -> (head xs, length xs)) . group . sort $ bs
  in sum . map (\a -> fromMaybe 0 (IM.lookup a factors) * a) $ as

main :: IO ()
main = do
  [inputl, inputr] <- transpose . map (map read . words) . lines <$> readFile "inputs/day01"
  print $ part1 inputl inputr
  print $ part2 inputl inputr
