module Lib where

import Control.Parallel.Strategies (rpar, parMap)
import Data.Array (listArray, Array, (!), assocs, bounds, (//), range, array)
import Data.IORef
import Control.Monad (forM_)

-- type MutableArray = IORef (Array (Int, Int) Int)

-- data Scoring = Scoring
--   { matchScore :: Int
--   , mismatchPenalty :: Int
--   , gapPenalty :: Int
--   }

-- -- Define the scoring function
-- score :: Scoring -> Char -> Char -> Int
-- score scoring a b
--   | a == b    = matchScore scoring
--   | otherwise = -mismatchPenalty scoring

-- initializeMutableArray :: IO MutableArray
-- initializeMutableArray n = newIORef $ listArray ((0, 0), (n-1, n-1)) $ repeat 0


-- updateMutableArray :: MutableArray -> (Int, Int) -> Int -> IO ()
-- updateMutableArray mutableArray index newValue = do
--   oldArray <- readIORef mutableArray
--   let newArray = oldArray // [(index, newValue)]
--   writeIORef mutableArray newArray

-- readIndex :: MutableArray -> (Int, Int) -> IO Int
-- readIndex mutableArray index = do
--   currentArray <- readIORef mutableArray
--   return (currentArray ! index)

-- antidiagonalIndices :: Int -> [[(Int, Int)]]
-- antidiagonalIndices n =
--   [ [(i, k - i) | i <- [0..k], k - i < n, k - i >= 0 && i < n] | k <- [0..2*(n-1)] ]

-- adiagonal :: Scoring -> String -> String -> IO MutableArray
-- adiagonal scoring s1 s2 = do
--   let n = length s1
--       m = length s2
--   mutableArray <- initializeMutableArray n
--   diagonals <- return $ antidiagonalIndices n

--   forM_ diagonals $ \diag -> do
--     scores <- readIORef mutableArray
--     forM_ diag $ \(i, j) -> do
--       let newScore
--             | i == 0 = -j * gapPenalty scoring
--             | j == 0 = -i * gapPenalty scoring
--             | otherwise = maximum
--                             [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
--                             , scores ! (i, j-1) - gapPenalty scoring
--                             , scores ! (i-1, j) - gapPenalty scoring
--                             ]
--       updateMutableArray mutableArray (i, j) newScore

--   return mutableArray


-- row :: Scoring -> String -> String -> Array (Int, Int) Int
-- row scoring s1 s2 =
--   let n = length s1
--       m = length s2
--       -- Function to calculate score for a cell (i, j)
--       calculateScore i j
--         | i == 0 = -j * gapPenalty scoring
--         | j == 0 = -i * gapPenalty scoring
--         | otherwise = maximum
--                         [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
--                         , scores ! (i, j-1) - gapPenalty scoring
--                         , scores ! (i-1, j) - gapPenalty scoring
--                         ]

--       -- Compute rows in parallel
--       scores = array ((0, 0), (n, m)) $ do
--         let row i = [((i, j), calculateScore i j) | j <- [0..m]]
--         concat $ parMap rpar row [0..m]

--   in scores

-- column :: Scoring -> String -> String -> Array (Int, Int) Int
-- column scoring s1 s2 =
--   let n = length s1
--       m = length s2
--       -- Function to calculate score for a cell (i, j)
--       calculateScore i j
--         | i == 0 = -j * gapPenalty scoring
--         | j == 0 = -i * gapPenalty scoring
--         | otherwise = maximum
--                         [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
--                         , scores ! (i, j-1) - gapPenalty scoring
--                         , scores ! (i-1, j) - gapPenalty scoring
--                         ]

--       -- Compute rows in parallel
--       scores = array ((0, 0), (n, m)) $ do
--         let column j = [((i, j), calculateScore i j) | i <- [0..n]]
--         concat $ parMap rpar column [0..n]

--   in scores


-- sequential :: Scoring -> String -> String -> Array (Int, Int) Int
-- sequential scoring s1 s2 =
--     let n = length s1
--         m = length s2
--         indexes = [(i, j) | i <- [0..n], j <- [0..m]]

--         calculateScore (i, j)
--           | i == 0    = -j * gapPenalty scoring
--           | j == 0    = -i * gapPenalty scoring
--           | otherwise = maximum
--                             [ scores ! (i-1, j-1) + score scoring (s1 !! (i-1)) (s2 !! (j-1))
--                             , scores ! (i, j-1) - gapPenalty scoring
--                             , scores ! (i-1, j) - gapPenalty scoring
--                             ]
--         scores = listArray ((0, 0), (n, m)) $ map calculateScore indexes
--     in scores

-- -- Traceback to get the aligned sequences
-- traceback :: Array (Int, Int) Int -> String -> String -> (String, String)
-- traceback scores s1 s2 = go (n, m) ("", "")
--   where
--     (n, m) = snd $ bounds scores

--     go (i, j) (align1, align2)
--       | i == 0 && j == 0 = (align1, align2)
--       | i > 0 && scores ! (i, j) == scores ! (i-1, j) - 1 =
--         let newAlign1 = s1 !! (i-1) : align1
--             newAlign2 = '-' : align2
--         in go (i-1, j) (newAlign1, newAlign2)
--       | j > 0 && scores ! (i, j) == scores ! (i, j-1) - 1 =
--         let newAlign1 = '-' : align1
--             newAlign2 = s2 !! (j-1) : align2
--         in go (i, j-1) (newAlign1, newAlign2)
--       | otherwise =
--         let newAlign1 = s1 !! (i-1) : align1
--             newAlign2 = s2 !! (j-1) : align2
--         in go (i-1, j-1) (newAlign1, newAlign2)