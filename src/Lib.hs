module Lib where

import Data.Array ((!), array, bounds, listArray, Array )
import Control.Parallel.Strategies (rpar, parMap)
import Data.Array.IO
    ( IOUArray, MArray(newArray), writeArray, freeze, readArray )
import Control.Concurrent.Async (mapConcurrently)

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

writeIndicesToScores :: IOUArray (Int, Int) Int -> (Int, Int) -> Int -> IO ()
writeIndicesToScores = writeArray

calculateScoreinDiagonal :: IOUArray (Int, Int) Int -> Scoring -> Array Int Char -> Array Int Char -> Int -> Int -> IO Int
calculateScoreinDiagonal scores scoring s1 s2 i j
  | i == 0    = return $ -j * gapPenalty scoring
  | j == 0    = return $ -i * gapPenalty scoring
  | otherwise = do
      scoreDiag <- readArray scores (i - 1, j - 1)
      scoreUp   <- readArray scores (i - 1, j)
      scoreLeft <- readArray scores (i, j - 1)
      let matchOrMismatchScore = if s1 ! (i - 1) == s2 ! (j - 1)
                                 then matchScore scoring
                                 else -mismatchPenalty scoring
      return $ maximum [ scoreDiag + matchOrMismatchScore
                       , scoreUp   - gapPenalty scoring
                       , scoreLeft - gapPenalty scoring
                       ]


computeDiagonal :: IOUArray (Int, Int) Int -> [(Int, Int)] -> Scoring -> Array Int Char -> Array Int Char -> IO ()
computeDiagonal scores stripe scoring s1 s2 = do
    let calculateAndUpdate (i, j) = do
            calculatedScore <- calculateScoreinDiagonal scores scoring s1 s2 i j
            writeIndicesToScores scores (i, j) calculatedScore
    _ <- mapConcurrently calculateAndUpdate stripe
    return ()

adiagonal :: Scoring -> String -> String -> IO (Array (Int, Int) Int)
adiagonal scoring s1 s2 = do
    let n = length s1
        m = length s2
        diags = antidiagonalIndices (max n m)
        s1Array = listArray (0, length s1 - 1) s1
        s2Array = listArray (0, length s2 - 1) s2

    scores <- newArray ((0, 0), (n - 1, m - 1)) 0 :: IO (IOUArray (Int, Int) Int)
    mapM_ (\diag -> computeDiagonal scores diag scoring s1Array s2Array) diags

    frozenScores <- freeze scores
    return frozenScores

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

