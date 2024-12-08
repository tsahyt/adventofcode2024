{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import qualified Data.Map.Strict as M
import Data.List (sortOn, groupBy, nub)
import Data.Function

parse :: String -> M.Map (Int, Int) Char
parse =
  M.fromList .
  concatMap (\(i, x) -> zipWith (\j c -> ((i, j), c)) [0..] x) .
  zip [0..] . lines

extent :: M.Map (Int, Int) a -> (Int, Int)
extent x =
  let
    (rows, cols) = unzip . M.keys $ x
  in (maximum rows, maximum cols)

invert :: Ord b => M.Map a b -> M.Map b [a]
invert =
  M.fromList . map (\xs -> (snd . head $ xs, map fst xs)) .
  groupBy ((==) `on` snd) . sortOn snd . M.toList

allDiffPairs :: Ord a => [a] -> [(a,a)]
allDiffPairs xs = [(x, y) | x <- xs, y <- xs, x < y]

antiNodes :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antiNodes (ax, ay) (bx, by) =
  [
    ( 2 * bx - ax, 2 * by - ay ),
    ( 2 * ax - bx, 2 * ay - by )
  ]

resonantAntiNodes :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
resonantAntiNodes n (ax, ay) (bx, by) =
  [(ax + i * (bx - ax), ay + i * (by - ay)) | i <- [-n .. n]]

solve :: ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> M.Map (Int, Int) Char -> Int
solve antis input =
  length . nub . concat . M.elems .
  M.map (concatMap (filter inBounds . uncurry antis) . allDiffPairs) .
  M.delete '.' . invert $ input
  where
    (maxX, maxY) = extent input
    inBounds (x, y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY

part1 :: M.Map (Int, Int) Char -> Int
part1 = solve antiNodes

part2 :: M.Map (Int, Int) Char -> Int
part2 input = solve (resonantAntiNodes n) input
  where
    n = uncurry max $ extent input

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day08"
  print $ part1 input
  print $ part2 input
