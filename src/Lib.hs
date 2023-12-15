module Lib where
import Control.Parallel.Strategies ( parMap, rpar )
import Data.Array ( Array, (!), bounds, array )

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

needlemanWunsch :: Scoring -> String -> String -> Array (Int, Int) Int
needlemanWunsch scoring s1 s2 =
  let n = length s1
      m = length s2
      -- Function to calculate score for a cell (i, j)
      calculateScore i j
        | i == 0 = -j * gapPenalty scoring
        | j == 0 = -i * gapPenalty scoring
        | otherwise = maximum
                        [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
                        , scores ! (i, j-1) - gapPenalty scoring
                        , scores ! (i-1, j) - gapPenalty scoring
                        ]

      -- Compute rows in parallel
      scores = array ((0, 0), (n, m)) $ do
        let columnOp j = [((i, j), calculateScore i j) | i <- [0..n]]
        concat $ parMap rpar columnOp [0..m]

  in scores


-- -- Traceback to get the aligned sequences
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