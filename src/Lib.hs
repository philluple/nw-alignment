module Lib where

-- import Control.Parallel.Strategies (rpar, parMap)
import Data.Array (listArray, Array, (!), assocs, bounds, (//), array, range, elems)
import Control.Monad.ST
import Data.Array.ST
import Control.Parallel.Strategies

data Scoring = Scoring
  { matchScore :: Int
  , mismatchPenalty :: Int
  , gapPenalty :: Int
  }

printMatrix :: Array (Int, Int) Int -> String
printMatrix arr =
  let ((rMin, cMin), (rMax, cMax)) = bounds arr
      rows = [[arr ! (i, j) | j <- [cMin..cMax]] | i <- [rMin..rMax]]
  in unlines $ map (unwords . map show) rows

score :: Scoring -> Char -> Char -> Int
score scoring a b
  | a == b    = matchScore scoring
  | otherwise = -mismatchPenalty scoring

antidiagonalIndices :: Int -> [(Int, Int)]
antidiagonalIndices n =
  concat [ [(i, k - i) | i <- [0..k], k - i < n, k - i >= 0 && i < n] | k <- [0..2*(n-1)] ]



adiagonal :: Scoring -> String -> String -> Array (Int, Int) Int
adiagonal scoring s1 s2 =
    let n = length s1
        m = length s2
        diagonals = antidiagonalIndices n
        
        calculateScore :: STUArray s (Int, Int) Int -> (Int, Int) -> ST s Int
        calculateScore scores (i, j)
          | i == 0 = return $ -j * gapPenalty scoring
          | j == 0 = return $ -i * gapPenalty scoring
          | otherwise = do
                val1 <- readArray scores (i-1, j-1)
                val2 <- readArray scores (i, j-1)
                val3 <- readArray scores (i-1, j)
                let diagScore = val1 + score scoring (s1 !! (i-1)) (s2 !! (j-1))
                let leftScore = val2 - gapPenalty scoring
                let upScore = val3 - gapPenalty scoring
                return $ maximum [diagScore, leftScore, upScore]
                
    in runST $ do
        scores <- newArray ((0, 0), (n-1, m-1)) 0 :: ST s (STUArray s (Int, Int) Int)
        
        forM_ diagonals $ \diagonalIndices ->
            forM_ diagonalIndices $ \(i,j) -> do
                scoreVal <- calculateScore scores (i, j)
                writeArray scores (i, j) scoreVal
        
        freeze scores



row :: Scoring -> String -> String -> Array (Int, Int) Int
row scoring s1 s2 =
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
        let rowCompute i = [((i, j), calculateScore i j) | j <- [0..m]]
        concat $ parMap rpar rowCompute [0..m]

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
        let columnCompute j = [((i, j), calculateScore i j) | i <- [0..n]]
        concat $ parMap rpar columnCompute [0..n]

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
        scores = listArray ((0, 0), (n, m)) $ map calculateScore indexes
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