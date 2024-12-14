{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Data.Char (isDigit)
import Data.List (sort, group)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP

import qualified Data.Set as S

data Robot = Robot
  {
    robotPosition :: !(Int, Int),
    robotVelocity :: !(Int, Int)
  } deriving (Show)

parse :: String -> [Robot]
parse = fst . last . readP_to_S (sepBy1 robot (char '\n'))
  where
    decimal = read <$> many1 (satisfy (\x -> isDigit x || x == '-'))
    robot = Robot <$> position <*> (char ' ' *> velocity)
    position = string "p=" *> pair
    velocity = string "v=" *> pair
    pair = (,) <$> (decimal <* char ',') <*> decimal

move :: Int -> Robot -> (Int, Int)
move seconds Robot{..} =
  (fst robotPosition + seconds * fst robotVelocity,
   snd robotPosition + seconds * snd robotVelocity)

wrap :: Int -> Int -> (Int, Int) -> (Int, Int)
wrap m n (x, y) = (x `mod` m, y `mod` n)

data Quadrant = NW | NE | SE | SW deriving (Eq, Ord, Show)

quadrant :: Int -> Int -> (Int, Int) -> Maybe Quadrant
quadrant m n p =
  case (compare x (m `div` 2), compare y (n `div` 2)) of
    (LT, LT) -> Just SW
    (LT, GT) -> Just SE
    (GT, GT) -> Just NE
    (GT, LT) -> Just NW
    _middle -> Nothing
  where
    (x, y) = wrap m n p

part1 :: [Robot] -> Int
part1 = product . map length . group . sort . mapMaybe (quadrant 101 103 . move 100)

-- Assume that topaz generates the puzzle input such that the christmas tree is
-- already there, then runs the robots backwards for some random number of
-- steps. Moreover, in that starting position no two robots occupy the same cell.
part2 :: [Robot] -> Int
part2 xs = fst . head . dropWhile positionsDiffer . zip [0..] . iterate (map $ step 101 103) $ xs
  where
    nrobots = length xs
    positionsDiffer (_, rs) = S.size (S.fromList (map robotPosition rs)) /= nrobots
    step m n r = r { robotPosition = wrap m n (move 1 r) }

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day14"
  print $ part1 input
  print $ part2 input
