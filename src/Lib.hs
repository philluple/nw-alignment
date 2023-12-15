module Lib where

import Control.Parallel.Strategies (rpar, parMap)
import Data.Array (listArray, Array, (!), assocs, bounds, (//), range)


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
          | otherwise = maximum
                        [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
                        , scores ! (i, j-1) - gapPenalty scoring
                        , scores ! (i-1, j) - gapPenalty scoring
                        ]

        scores = listArray ((0, 0), (n-1, m-1)) $ parMap rpar calculateScore (range ((0, 0), (n-1, m-1)))

        updateDiagonal :: Array (Int, Int) Int -> [(Int, Int)] -> Array (Int, Int) Int
        updateDiagonal arr diag = arr // assocs (listArray (head diag, last diag) $ map (arr !) diag)

        updatedScores = foldl updateDiagonal scores diagonals

    in updatedScores


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