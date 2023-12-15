module Lib where

import Control.Parallel.Strategies (rpar, using, parMap)
import Data.Array ( (!), (//), bounds, listArray, Array, assocs )


data Scoring = Scoring
  { matchScore :: Int
  , mismatchPenalty :: Int
  , gapPenalty :: Int
  }

-- Define the scoring function
score :: Scoring -> Char -> Char -> Int
score scoring a b
  | a == b    = matchScore scoring
  | otherwise = -mismatchPenalty scoring

antidiagonalIndices :: Int -> [[(Int, Int)]]
antidiagonalIndices n =
  [ [(i, k - i) | i <- [0..k], k - i < n, k - i >= 0 && i < n] | k <- [0..2*(n-1)] ]

needlemanWunsch :: Scoring -> String -> String -> Array (Int, Int) Int
needlemanWunsch scoring s1 s2 =
    let n = length s1
        m = length s2
        diagonals = antidiagonalIndices n

        calculateScore :: (Int, Int) -> Int
        calculateScore (i, j)
          | i == 0 = -j * gapPenalty scoring
          | j == 0 = -i * gapPenalty scoring
          | otherwise = scores ! (i - 1, j - 1)

        calcDiag :: [(Int, Int)] -> Array (Int, Int) Int
        calcDiag diagonal = listArray ((head diagonal), (last diagonal)) $ parMap rpar calculateScore diagonal

        scores = foldl (\acc row -> calcDiag row `seq` acc // assocs (calcDiag row)) (listArray ((0, 0), (n, m)) $ repeat 0) diagonals
    in scores

-- Traceback to get the aligned sequences
traceback :: Array (Int, Int) Int -> String -> String -> (String, String)
traceback scores s1 s2 = go (n, m) ("", "")
  where
    (n, m) = snd $ bounds scores

    go (i, j) (align1, align2)
      | i == 0 && j == 0 = (align1, align2)
      | i > 0 && scores ! (i, j) == scores ! (i-1, j) - 1 =
        let newAlign1 = s1 !! (i-1) : align1
            newAlign2 = '-' : align2
        in go (i-1, j) (newAlign1, newAlign2)
      | j > 0 && scores ! (i, j) == scores ! (i, j-1) - 1 =
        let newAlign1 = '-' : align1
            newAlign2 = s2 !! (j-1) : align2
        in go (i, j-1) (newAlign1, newAlign2)
      | otherwise =
        let newAlign1 = s1 !! (i-1) : align1
            newAlign2 = s2 !! (j-1) : align2
        in go (i-1, j-1) (newAlign1, newAlign2)