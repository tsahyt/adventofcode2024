module Main where

import Data.Char
import Data.Foldable (find)
import Data.List (sortBy)
import Text.ParserCombinators.ReadP

data Constraint = Before !Int !Int
  deriving Show

parse :: String -> ([Constraint], [[Int]])
parse = fst . last . readP_to_S problem
  where
    decimal = read <$> many1 (satisfy isDigit)

    constraint = Before <$> (decimal <* char '|') <*> decimal

    problem = do
      cs <- sepBy1 constraint (char '\n')
      skipSpaces
      us <- sepBy1 (sepBy1 decimal (char ',')) (char '\n')
      pure (cs, us)

pageSort :: [Constraint] -> [Int] -> [Int]
pageSort cs = sortBy go
  where
    go a b
      | Just _ <- find (\(Before x y) -> x == a && y == b) cs = LT
      | otherwise = GT

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

part1 :: [Constraint] -> [[Int]] -> Int
part1 cs =
  sum . map middle . filter (\x -> x == pageSort cs x)

part2 :: [Constraint] -> [[Int]] -> Int
part2 cs =
  sum . map (middle . snd) . filter (not . fst) .
  map (\x -> let x' = pageSort cs x in (x == x', x'))

main :: IO ()
main = do
  (constraints, updates) <- parse <$> readFile "inputs/day05"
  print $ part1 constraints updates
  print $ part2 constraints updates
