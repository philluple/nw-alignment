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

calcuateScores scoring s1 s2 = 
    let n = length s1
        m = length s2
        indices = [(i, j) | i <- [0..n], j <- [0..m]]

        calculateScore (i, j)
          | i == 0    = -j * gapPenalty scoring
          | j == 0    = -i * gapPenalty scoring
          | otherwise = maximum 
                            [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
                            , scores ! (i, j-1) - gapPenalty scoring
                            , scores ! (i-1, j) - gapPenalty scoring
                            ]
        scores = listArray ((0, 0), (n, m)) $ parMap rpar calculateScore indices
    in scores

-- -- Needleman-Wunsch seq algorithm
needlemanWunsch :: Scoring -> String -> String -> Array (Int, Int) Int
needlemanWunsch scoring s1 s2 = 
	calcuateScores scoring s1 s2

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


-- Example usage with different penalties

  -- putStrLn $ "Sequence 1: " ++ sequence1
  -- putStrLn $ "Sequence 2: " ++ sequence2
  -- putStrLn $ "Alignment 1: " ++ alignment1
  -- putStrLn $ "Alignment 2: " ++ alignment2


  -- Parallelized Needleman-Wunsch algorithm
-- needlemanWunsch :: Scoring -> String -> String -> Array (Int, Int) Int
-- needlemanWunsch scoring s1 s2 =
--   let n = length s1
--       m = length s2

--       -- Initialize the score matrix
--       scores = listArray ((0, 0), (n, m)) [calculateScore i j | i <- [0..n], j <- [0..m]]

--       -- Function to calculate scores for a diagonal
--       calculateDiagonal i j
--         | i == 0    = [-j * gapPenalty scoring]
--         | j == 0    = [-i * gapPenalty scoring]
--         | otherwise = parMap rpar (calculateScore i) [j, j-1 .. 0]

--       calculateScore i j = maximum
--         [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
--         , scores ! (i, j-1) - gapPenalty scoring
--         , scores ! (i-1, j) - gapPenalty scoring
--         ]

--       -- Calculate scores for each diagonal in parallel
--       diagonals = parMap rpar (\k -> calculateDiagonal (min k n) (k - min k n)) [0..n+m-2]

--       -- Update the score matrix with the calculated diagonals
--       updatedScores = foldl (\arr (i, diagonal) -> arr // zip [(i, j) | j <- [0..length diagonal - 1]] diagonal) scores (zip [0..] diagonals)

--   in updatedScores