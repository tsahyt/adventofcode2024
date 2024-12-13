{-# LANGUAGE RecordWildCards #-}

module Main where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Bifunctor

data Machine = Machine
  {
    buttonA :: !(Int, Int),
    buttonB :: !(Int, Int),
    prize :: !(Int, Int)
  }
  deriving Show

parse :: String -> [Machine]
parse = fst . last . readP_to_S (machine `sepBy1` string "\n\n")
  where
    machine = do
      buttonA <- string "Button A: " *> button <* char '\n'
      buttonB <- string "Button B: " *> button <* char '\n'
      prize <- string "Prize: " *> prize'
      pure $ Machine{..}

    button = (,) <$> (string "X+" *> decimal) <*> (string ", Y+" *> decimal)
    prize' = (,) <$> (string "X=" *> decimal) <*> (string ", Y=" *> decimal)
    decimal = read <$> many1 (satisfy isDigit)

-- n.b. not jim cramer. answer need not be inverted
cramer :: Int -> Int -> Int -> Int -> Int -> Int -> (Double, Double)
cramer a1 b1 c1 a2 b2 c2 =
  let
    d = det2 a1 b1 a2 b2
    dx = det2 c1 b1 c2 b2
    dy = det2 a1 c1 a2 c2
  in (fromIntegral dx / fromIntegral d, fromIntegral dy / fromIntegral d)
  where
    det2 a b c d = a * d - b * c

solve :: Machine -> Maybe (Int, Int)
solve Machine{..} =
  let
    (a, b) =
      cramer
      (fst buttonA) (fst buttonB) (fst prize)
      (snd buttonA) (snd buttonB) (snd prize)
    (a', b') = (truncate a, truncate b)
  in if fromIntegral a' == a && fromIntegral b' == b then Just (a', b') else Nothing

part1 :: [Machine] -> Int
part1 = sum . map (maybe 0 score . solve)
  where
    score (a, b) = 3 * a + b

part2 :: [Machine] -> Int
part2 = part1 . map big
  where
    big m = m { prize = bimap grow grow (prize m) }
    grow = (+ 10000000000000)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day13"
  print $ part1 input
  print $ part2 input
