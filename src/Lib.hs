module Lib where

import Control.Parallel.Strategies (using, parListChunk, rpar, parMap)
import Data.Array (Array, (!), array, listArray, bounds)
import qualified Data.Vector as V
import Data.List (foldl')

data Scoring = Scoring
  { matchScore :: Int
  , mismatchPenalty :: Int
  , gapPenalty :: Int
  }

score :: Scoring -> Char -> Char -> Int
score scoring a b
  | a == b    = matchScore scoring
  | otherwise = -mismatchPenalty scoring

stringToVector :: String -> V.Vector Char
stringToVector = V.fromList

convertDiagonalsTo2DArray :: [Array Int Int] -> Array (Int, Int) Int
convertDiagonalsTo2DArray diagonals = array size elements
  where
    n = length (head diagonals) - 1  
    m = length (last diagonals) - 1  
    size = ((0, 0), (n, m))

    indexedDiagonal = zip [0..] (reverse diagonals)
    elements = concatMap generateElements indexedDiagonal

    generateElements (diagIndex, diagonal) =
      let start = max 0 (diagIndex - m)  
          end = min n diagIndex          
          offset = diagIndex - start     
      in [ ((i, diagIndex - i), diagonal ! (i - start + offset)) | i <- [start..end] ]


antidiagonal :: String -> String -> [Array Int Int]
antidiagonal firstStr secondStr =
  let n = length firstStr
      m = length secondStr
      seq1 = stringToVector firstStr
      seq2 = stringToVector secondStr
      base = [array (0, 1) [(0, -1), (1, -1)], array (0, 0) [(0, 0)]]
  in foldl' (createDiagonal seq1 seq2 n m) base [2..n+m]



createDiagonal :: V.Vector Char -> V.Vector Char -> Int -> Int -> [Array Int Int] -> Int -> [Array Int Int]
createDiagonal seq1 seq2 n m diagonals i = case diagonals of
    (currentDiagonal:prevDiagonal:_) ->
        let newDiagonal = calcForDiagonal seq1 seq2 prevDiagonal currentDiagonal i n m
        in newDiagonal `seq` (newDiagonal : diagonals)
    _ -> error "createDiagonal: Less than two diagonals provided"

calcForDiagonal :: V.Vector Char -> V.Vector Char -> Array Int Int -> Array Int Int -> Int -> Int -> Int -> Array Int Int
calcForDiagonal sec1 sec2 prevDiag1 prevDiag2 counter n m =
  let indices = if counter <= m then [0..counter] else [counter - m..n]
      chunkSize = 10
      scoreList = map (calculateValue sec1 sec2 prevDiag1 prevDiag2 counter n m) indices `using` parListChunk chunkSize rpar
  in listArray (0, length scoreList - 1) scoreList



calculateValue :: V.Vector Char -> V.Vector Char -> Array Int Int -> Array Int Int -> Int -> Int -> Int -> Int -> Int
calculateValue sec1 sec2 prevDiag1 prevDiag2 counter n m i =
  let inBounds1 = i > 0 && i <= n
      inBounds2 = counter - i > 0 && counter - i <= m
      matchOrMismatch = if inBounds1 && inBounds2 
                        then if (sec1 V.! (i - 1)) == (sec2 V.! (counter - i - 1))
                            then 1 
                            else -2
                        else 0
      scoreDiag = if i > 0 && i - 1 < length prevDiag2 
                  then (prevDiag2 ! (i - 1)) + matchOrMismatch 
                  else -1
      scoreUp = if i < length prevDiag1 
                then (prevDiag1 ! i) - 1 
                else -1
      scoreLeft = if i > 0 && i - 1 < length prevDiag1 
                  then (prevDiag1 ! (i - 1)) - 1 
                  else -1
  in maximum [scoreDiag, scoreUp, scoreLeft]

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

