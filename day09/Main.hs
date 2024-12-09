module Main where

import Control.Monad.ST
import Control.Monad (forM_)
import Data.Char (digitToInt, isDigit)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Set as S

fsMap :: String -> V.Vector Int
fsMap z = V.fromList . concat $ go (filter isDigit z) True 0
  where
    go [] _ _ = []
    go (x:xs) True i = replicate (digitToInt x) i : go xs False (i + 1)
    go (x:xs) False i = replicate (digitToInt x) (-1) : go xs True i

isFree :: Int -> Bool
isFree (-1) = True
isFree _ = False

fragment :: V.Vector Int -> V.Vector Int
fragment disk = runST $ do
  x <- V.thaw disk
  go 0 (M.length x - 1) x
  V.freeze x

  where
    go l r x
      | l >= r = M.swap x l r
      | otherwise = do
          l' <- scanTo l (< 0) (+ 1) x
          r' <- scanTo r (> 0) (subtract 1) x
          M.swap x l' r'
          go l' r' x

compact :: V.Vector Int -> V.Vector Int
compact disk = runST $ do
  x <- V.thaw disk
  r <- scanTo (M.length x - 1) (not . isFree) (subtract 1) x
  go r x S.empty
  V.freeze x

  where
    go r x processed
      | r <= 0 = pure ()
      | otherwise = do
          r' <- startOfBlock r x
          fileId <- M.read x r'
          rNext <- scanTo (r' - 1) (not . isFree) (subtract 1) x

          if S.member fileId processed || fileId == 0
            then do
              go rNext x processed
            else do
              let n = r - r' + 1
              l <- firstFree 0 r' n x
              let processed' = S.insert fileId processed

              case l of
                Nothing -> do
                  go rNext x processed'
                Just l' -> do
                  forM_ [0 .. n - 1] $ \i ->
                    M.swap x (l' + i) (r' + i)

                  go rNext x processed'

    firstFree s r n x
      | s > r = pure Nothing
      | s + n >= M.length x = pure Nothing
      | otherwise = do
          s' <- scanTo s isFree (+ 1) x
          allFree <- all isFree <$> mapM (M.read x) [s' .. s' + n - 1]
          if allFree
            then pure (Just s')
            else firstFree (s' + n) r n x

    startOfBlock p x = do
      v <- M.read x p
      (+ 1) <$> scanTo p (/= v) (subtract 1) x

scanTo :: forall s. Int -> (Int -> Bool) -> (Int -> Int) -> M.MVector s Int -> ST s Int
scanTo from check step x
  | from < 0 = pure 0
  | otherwise = do
      val <- M.read x from
      if check val
        then pure from
        else scanTo (step from) check step x

checksum :: V.Vector Int -> Int
checksum fs = sum . map (uncurry (*)) . filter ((>= 0) . snd) $ zip [0..] (V.toList fs)

part1 :: V.Vector Int -> Int
part1 = checksum . fragment

part2 :: V.Vector Int -> Int
part2 = checksum . compact

main :: IO ()
main = do
  input <- fsMap <$> readFile "inputs/day09"
  print $ part1 input
  print $ part2 input
