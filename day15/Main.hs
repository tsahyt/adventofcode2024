{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Foldable
import Linear.V2
import Linear.Vector

import qualified Data.Map.Strict as M

type Pos = V2 Int

movePos :: Pos -> Move -> Pos
movePos x m = x ^+^ moveDir m
  where
    moveDir North = V2 (-1) 0
    moveDir East = V2 0 1
    moveDir South = V2 1 0
    moveDir West = V2 0 (-1)

data Tile
  = Box
  | Robot
  | Wall
  deriving (Show, Eq)

data Move
  = North | East | South | West
  deriving (Show)

horizontal, vertical :: Move -> Bool
horizontal East = True
horizontal West = True
horizontal _ = False
vertical = not . horizontal

parse :: String -> (M.Map Pos Tile, [Move])
parse xs = (map', moves)
  where
    (maplines, moveline) = fmap (concat . tail) . span (/= "") . lines $ xs
    map' = M.mapMaybe toTile .  M.fromList .
      concatMap (\(i, x) -> zipWith (\j c -> (V2 i j, c)) [0..] x) . zip [0..] $ maplines
    moves = map toMove moveline

    toTile '#' = Just Wall
    toTile '@' = Just Robot
    toTile 'O' = Just Box
    toTile _ = Nothing

    toMove '^' = North
    toMove 'v' = South
    toMove '>' = East
    toMove '<' = West
    toMove c = error $ "impossible move: " <> pure c

move :: M.Map Pos Tile -> Move -> M.Map Pos Tile
move mp x =
  let robot = maybe (error "nobot") fst . find ((== Robot) . snd) . M.assocs $ mp
  in snd $ go mp robot
  where
    go :: M.Map Pos Tile -> Pos -> (Bool, M.Map Pos Tile)
    go m p
      | Nothing <- M.lookup (movePos p x) m = (True, M.insert (movePos p x) (m M.! p) (M.delete p m))
      | Just Wall <- M.lookup (movePos p x) m = (False, m)
      | otherwise =
        let (moved, m') = go m (movePos p x)
        in if moved then go m' p else (False, m')

gpsScore :: M.Map Pos Tile -> Int
gpsScore = sum . map (\(V2 i j) -> 100 * i + j) . M.keys . M.filter (== Box)

part1 :: M.Map Pos Tile -> [Move] -> Int
part1 m = gpsScore . foldl' move m

main :: IO ()
main = do
  (inputMap, inputMoves) <- parse <$> readFile "inputs/day15"
  print $ part1 inputMap inputMoves
