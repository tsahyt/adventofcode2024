module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable

parse :: String -> M.Map (Int, Int) Char
parse =
  M.fromList .
  concatMap (\(i, x) -> zipWith (\j c -> ((i, j), c)) [0..] x) .
  zip [0..] . lines

bft :: (Foldable t, Ord a)
    => (a -> t a)
    -> t a
    -> [a]
bft suc = go S.empty . toList
    where go _ [] = []
          go visited (n:ns)
              | n `S.member` visited = go visited ns
              | otherwise = n : go (n `S.insert` visited)
                                   (ns ++ toList (suc n))

regions :: M.Map (Int, Int) Char -> [S.Set (Int, Int)]
regions m = S.toList . S.fromList $ map (S.fromList . bft neighbour . pure) (M.keys m)
  where
    neighbour x@(i, j) =
      filter (\y -> M.lookup x m == M.lookup y m)
      [(i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1)]

contiguous :: S.Set (Int, Int) -> S.Set (S.Set (Int, Int))
contiguous s = S.fromList . map (S.fromList . bft neighbour . pure) . S.toList $ s
  where
    neighbour (i, j) =
      filter (`S.member` s)
      [(i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1)]

outside :: S.Set (Int, Int) -> [S.Set (Int, Int)]
outside r = map neighborsAt [(0, 1), (1, 0), (0, -1), (-1, 0)]
  where
    neighborsAt (c, d) = S.map (\(a, b) -> (a + c, b + d)) r `S.difference` r

part1 :: M.Map (Int, Int) Char -> Int
part1 input = sum $ do
  region <- regions input
  let border = outside region
  pure $ S.size region * sum (map S.size border)

part2 :: M.Map (Int, Int) Char -> Int
part2 input = sum $ do
  region <- regions input
  border <- outside region
  pure $ S.size region * S.size (contiguous border)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day12"
  print $ part1 input
  print $ part2 input
