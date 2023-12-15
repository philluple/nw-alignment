module Lib where
import Control.Parallel.Strategies (rpar, parMap, rseq)
import Data.Array

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

antidiagonalIndices :: Int -> [(Int, Int)]
antidiagonalIndices n =
  concat[ [(i, k - i) | i <- [0..k], k - i < n, k - i >= 0 && i < n] | k <- [0..2*(n-1)] ]

needlemanWunsch :: Scoring -> [(Int, Int)] -> String -> String -> Array (Int, Int) Int
needlemanWunsch scoring indices s1 s2 = 
    let n = length s1
        m = length s2
        indices = antidiagonalIndices (n)

        calculateScore (i, j)
          | i == 0    = -j * gapPenalty scoring
          | j == 0    = -i * gapPenalty scoring
          | otherwise = maximum 
                            [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
                            , scores ! (i, j-1) - 1
                            , scores ! (i-1, j) - 1
                            ]
        scores = listArray ((0, 0), (n, m)) $ parMap rpar calculateScore indices
    in scores
-- -- Needleman-Wunsch seq algorithm

-- Traceback to get the aligned sequences
traceback :: Array (Int, Int) Int -> String -> String -> (String, String)
traceback scores s1 s2 = go (n, m) ("", "")
  where
    (n, m) = snd $ bounds scores

    go (i, j) (align1, align2)
      | i == 0 && j == 0 = (align1, align2)
      | i > 0 && scores ! (i, j) == scores ! (i-1, j) - 1 =
        go (i-1, j) (s1 !! (i-1) : align1, '-' : align2)
      | j > 0 && scores ! (i, j) == scores ! (i, j-1) - 1 =
        go (i, j-1) ('-' : align1, s2 !! (j-1) : align2)
      | otherwise =
        go (i-1, j-1) (s1 !! (i-1) : align1, s2 !! (j-1) : align2)
