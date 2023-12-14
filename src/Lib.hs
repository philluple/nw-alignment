module Lib where

import Data.Array
import Control.Parallel.Strategies (rpar, parMap, rseq)
data Scoring = Scoring
  { matchScore :: Int
  , mismatchPenalty :: Int
  , gapPenalty :: Int
  }

score :: Scoring -> Char -> Char -> Int
score scoring a b
  | a == b    = matchScore scoring
  | otherwise = -mismatchPenalty scoring

antidiagonalIndices :: Int -> [[(Int, Int)]]
antidiagonalIndices n =
  [ [(i, k - i) | i <- [0..k], k - i < n, k - i >= 0 && i < n] | k <- [0..2*(n-1)] ]

calculateForAntiDiagonal :: Scoring -> Array (Int, Int) Int -> [(Int, Int)] -> Array (Int, Int) Int
calculateForAntiDiagonal scoring scores indices =
    let calculateSingleCell (i, j) =
            let calculatedScore = calculateScore scoring i j -- Calculate the score for (i, j)
            in calculatedScore `seq` (i, j, calculatedScore) -- Use `seq` to force calculation

        calculatedCells = map (\(i, j) -> calculateSingleCell (i, j)) indices -- Calculate for each index

        updatedScores = foldl (\arr (i, j, scoreVal) -> arr // [((i, j), scoreVal)]) scores calculatedCells -- Update the scores array
    in updatedScores

calculateScores :: Scoring -> String -> String -> Array (Int, Int) Int
calculateScores scoring s1 s2 =
  let n = length s1
      m = length s2
      antiDiagonals = antidiagonalIndices n
      for anti in antiDiagonals do
        
      calculateScore (i, j)
        | i == 0    = -j * gapPenalty scoring
        | j == 0    = -i * gapPenalty scoring
        | otherwise = maximum
                          [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
                          , scores ! (i, j-1) - gapPenalty scoring
                          , scores ! (i-1, j) - gapPenalty scoring
                          ]
      scores = listArray ((0, 0), (n, m)) $ parMap rpar calculateScore (concatMap id cells)
  in scores


needlemanWunsch :: Scoring -> String -> String -> Array (Int, Int) Int
needlemanWunsch scoring s1 s2 = 
    calculateScores scoring s1 s2

traceback :: Array (Int, Int) Int -> Scoring -> String -> String -> (String, String)
traceback scores scoring s1 s2 = go (n, m) ("", "")
  where
    (n, m) = snd $ bounds scores

    go (i, j) (align1, align2)
      | i == 0 && j == 0 = (align1, align2)
      | i > 0 && scores ! (i, j) == scores ! (i-1, j) - gapPenalty scoring =
        go (i-1, j) (s1 !! (i-1) : align1, '-' : align2)
      | j > 0 && scores ! (i, j) == scores ! (i, j-1) - gapPenalty scoring =
        go (i, j-1) ('-' : align1, s2 !! (j-1) : align2)
      | otherwise =
        go (i-1, j-1) (s1 !! (i-1) : align1, s2 !! (j-1) : align2)
