module Lib where

import Data.Array ((!), array, bounds, listArray, Array )
import Control.Parallel.Strategies (rpar, parMap)

data Scoring = Scoring
  { matchScore :: Int
  , mismatchPenalty :: Int
  , gapPenalty :: Int
  }

score :: Scoring -> Char -> Char -> Int
score scoring a b
  | a == b    = matchScore scoring
  | otherwise = -mismatchPenalty scoring

calculateDScore :: String -> String -> [[Int]]
calculateDScore s1 s2 =
  let n = length s1
      m = length s2
      initialDiagonals = [[0], [-1, -1]]  -- Initialize first two diagonalstack exec nw-alignment adiagonal ./input/100x100.txt -- +RTS -N8
  in foldl (updateDiagonals s1 s2 n m) initialDiagonals [2..n+m]

updateDiagonals :: String -> String -> Int -> Int -> [[Int]] -> Int -> [[Int]]
updateDiagonals s1 s2 n m previousDiagonals i =
  let newDiagonal = calculateScore s1 s2 (head previousDiagonals) (head (tail previousDiagonals)) i n m
  in [newDiagonal, head previousDiagonals]  -- Keep only the last two diagonals

calculateScore :: String -> String -> [Int] -> [Int] -> Int -> Int -> Int -> [Int]
calculateScore s1 s2 prevDiag1 prevDiag2 counter n m =
  let indices = if counter <= m then [0..counter] else [counter - m..n]
  in map (calculateValue s1 s2 prevDiag1 prevDiag2 counter n m) indices

calculateValue :: String -> String -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int
calculateValue s1 s2 prevDiag1 prevDiag2 counter n m i =
  let inBounds1 = i > 0 && i <= n
      inBounds2 = counter - i > 0 && counter - i <= m
      matchOrMismatch = if inBounds1 && inBounds2 && (s1 !! (i - 1)) == (s2 !! (counter - i - 1)) 
                        then 1 else -1
      scoreDiag = if i > 0 && i - 1 < length prevDiag2 then (prevDiag2 !! (i - 1)) + matchOrMismatch else -1
      scoreUp = if i < length prevDiag1 then (prevDiag1 !! i) - 1 else -1
      scoreLeft = if i > 0 && i - 1 < length prevDiag1 then (prevDiag1 !! (i - 1)) - 1 else -1
  in maximum [scoreDiag, scoreUp, scoreLeft]

-- calculateDScore :: String -> String -> [[Int]]
-- calculateDScore s1 s2 =
--   let n = length s1
--       diag1 = [0]
--       diag2 = [-1, -1]
--       initialArray = [[]]
--   in snd $ foldl (\i ->
--         let seq1
--               | i <= n = take i s1
--               | i == n + 1 = take n s1
--               | otherwise = drop (i - n + 1) s1
--             seq2
--               | i <= n = take i s2
--               | i == n + 1 = take n s2
--               | otherwise = drop (i - n + 1) s2
--             d1' = d2
--             d2' = calculateScore seq1 seq2 d1' d2 (n + 1)
--         in (d1', d2')
--       ) (diag1, diag2) [2..n+n+1]

-- calculateScore :: String -> String -> [Int] -> [Int] -> Int -> Int -> [Int]
-- calculateScore letterSequence1 letterSequence2 diag1 diag2 counter maxLen = newArray
--   where
--     newArray = parMap rpar calculateValue indices

--     indices
--       | counter < maxLen = [0..counter]
--       | otherwise = [0..maxLen - (counter - maxLen)]

--     calculateValue i
--       | i == 0 && counter < maxLen = counter * (-2)
--       | i == counter && counter < maxLen = counter * (-2)
--       | otherwise = maximum [diag2 !! (i-1) - 2,
--                               diag1 !! (i-1) + if letterSequence1 !! (i) == letterSequence2 !! (i) then 1 else (-1),
--                               diag2 !! i - 2]


row :: Scoring -> String -> String -> Array (Int, Int) Int
row scoring s1 s2 =
  let n = length s1
      m = length s2
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
        let computeRow i = [((i, j), calculateScore i j) | j <- [0..m]]
        concat $ parMap rpar computeRow [0..m]

  in scores

column :: Scoring -> String -> String -> Array (Int, Int) Int
column scoring s1 s2 =
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
        let computeColumn j = [((i, j), calculateScore i j) | i <- [0..n]]
        concat $ parMap rpar computeColumn [0..n]

  in scores


sequential :: Scoring -> String -> String -> Array (Int, Int) Int
sequential scoring s1 s2 =
    let n = length s1
        m = length s2
        indexes = [(i, j) | i <- [0..n], j <- [0..m]]

        calculateScore (i, j)
          | i == 0    = -j * gapPenalty scoring
          | j == 0    = -i * gapPenalty scoring
          | otherwise = maximum
                            [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
                            , scores ! (i, j-1) - gapPenalty scoring
                            , scores ! (i-1, j) - gapPenalty scoring
                            ]
        scores = listArray ((0, 0), (n, m)) $ parMap rpar calculateScore indexes
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

