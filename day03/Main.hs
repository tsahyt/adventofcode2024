module Main where

import Data.List
import Data.Char

data Instruction
  = Mul !Int !Int
  | Do
  | Donot
  deriving Show

parse :: String -> [Instruction]
parse [] = []
parse s
  | "do()" `isPrefixOf` s = Do : parse (drop 4 s)
  | "don't()" `isPrefixOf` s = Donot : parse (drop 7 s)
  | "mul(" `isPrefixOf` s =
    let
      s' = drop 4 s
      (x, s'') = span isDigit s'
      (y, s''') = span isDigit (tail s'')
    in if head s'' == ',' && head s''' == ')'
       then Mul (read x) (read y) : parse s'''
       else parse (tail s')
  | otherwise = parse (tail s)

part1 :: [Instruction] -> Int
part1 = sum . map go
  where
    go (Mul x y) = x * y
    go _ = 0

part2 :: [Instruction] -> Int
part2 = fst . foldl' go (0, True)
  where
    go (z, enabled) (Mul x y) = (z + if enabled then x * y else 0, enabled)
    go (z, _) Do = (z, True)
    go (z, _) Donot = (z, False)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day03"
  print $ part1 input
  print $ part2 input
