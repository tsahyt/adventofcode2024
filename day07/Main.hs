module Main where

import Text.ParserCombinators.ReadP
import Data.Char
import Control.Monad

parse :: String -> [(Int, [Int])]
parse = fst . last . readP_to_S (equation `sepBy1` char '\n')
  where
    decimal = read <$> many1 (satisfy isDigit)
    equation = do
      lhs <- decimal
      char ':' >> skipSpaces
      rhs <- decimal `sepBy1` char ' '
      pure (lhs, rhs)

(|||) :: Int -> Int -> Int
a ||| b = read (show a ++ show b)

eqs :: [a -> a -> a] -> [a] -> [a]
eqs possible xs = map (go xs) ops
  where
    ops = replicateM (length xs - 1) possible
    go (a1 : a2 : as) (f : fs) = go ((a1 `f` a2) : as) fs
    go [r] _ = r
    go _ _ = error "nope"

solve :: (Num a, Eq a) => [a -> a -> a] -> [(a, [a])] -> a
solve possible = sum . map fst . filter (\(testValue, nums) -> testValue `elem` eqs possible nums)

part1 :: [(Int, [Int])] -> Int
part1 = solve [(+), (*)]

part2 :: [(Int, [Int])] -> Int
part2 = solve [(+), (*), (|||)]

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day07"
  print $ part1 input
  print $ part2 input
