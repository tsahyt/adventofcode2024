module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable (find)
import Data.Maybe (fromMaybe)

parse :: String -> M.Map (Int, Int) Char
parse =
  M.fromList .
  concatMap (\(i, x) -> zipWith (\j c -> ((i, j), c)) [0..] x) .
  zip [0..] . lines

startingPos :: M.Map (Int, Int) Char -> (Int, Int)
startingPos = fst . fromMaybe (error "404 guard not found") . find ((== '^') . snd) . M.toList

data Direction = N | E | S | W
  deriving (Show, Ord, Eq)

neighbour :: (Int, Int) -> Direction -> (Int, Int)
neighbour (i, j) N = (i - 1, j)
neighbour (i, j) E = (i, j + 1)
neighbour (i, j) S = (i + 1, j)
neighbour (i, j) W = (i, j - 1)

turn :: Direction -> Direction
turn N = E
turn E = S
turn S = W
turn W = N

walk :: Direction -> (Int, Int) -> M.Map (Int, Int) Char -> S.Set (Int, Int) -> S.Set (Int, Int)
walk facing pos env visited
  | Nothing <- M.lookup pos env = visited
  | otherwise =
    let next = neighbour pos facing
    in case M.lookup next env of
        Just '#' -> walk (turn facing) pos env visited
        _noObstacle -> walk facing next env (S.insert pos visited)

loop :: Direction -> (Int, Int) -> M.Map (Int, Int) Char -> S.Set ((Int, Int), Direction) -> Bool
loop facing pos env visited
  | S.member (pos, facing) visited = True
  | Nothing <- M.lookup pos env = False
  | otherwise =
    let next = neighbour pos facing
    in case M.lookup next env of
        Just '#' -> loop (turn facing) pos env visited
        _noObstacle -> loop facing next env (S.insert (pos, facing) visited)

part1 :: M.Map (Int, Int) Char -> Int
part1 input = S.size $ walk N (startingPos input) input S.empty

part2 :: M.Map (Int, Int) Char -> Int
part2 input =
  let
    start = startingPos input
    obstacles = S.toList $ walk N start input S.empty
  in length $ filter (\o -> loop N start (M.insert o '#' input) S.empty) obstacles

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day06"
  print $ M.size input
  print $ part1 input
  print $ part2 input
