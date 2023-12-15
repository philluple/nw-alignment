import Lib
import System.Environment (getArgs)
import Data.Array
import Control.Parallel.Strategies (rpar, parMap, rseq)

printScores :: Array (Int, Int) Int -> IO ()
printScores arr = do
  let ((x1, y1), (x2, y2)) = bounds arr
  mapM_ (\i -> mapM_ (print . (arr !)) [(i, j) | j <- [y1..y2]]) [x1..x2]

getFiles :: String -> (FilePath, FilePath)
getFiles arg =
  case arg of
    "test1" -> ("./input/human1.txt", "./input/rat1.txt")
    "test2" -> ("./input/human2.txt", "./input/rat2.txt")
    "test3" -> ("./input/human3.txt", "./input/rat3.txt")
    "test4" -> ("./input/human4.txt", "./input/rat4.txt")
    _       -> ("./input/human1.txt", "./input/rat1.txt")

main :: IO ()
main = do
  args <- getArgs
  let (file1, file2) = case args of
        [input] -> getFiles input
        _       -> ("./input/human1.txt", "./input/rat1.txt")
  case args of
    ["test1"] -> putStrLn "Running test1..."
    ["test2"] -> putStrLn "Running test2..."
    ["test3"] -> putStrLn "Running test3..."
    ["test4"] -> putStrLn "Running test4..."
    [input]   -> putStrLn $ "Running test for input: " ++ input
    _         -> putStrLn "Invalid test case specified"

  seq1 <- readFile file1
  seq2 <- readFile file2

  let sequence1 = ' ' : seq1
      sequence2 = ' ' : seq2
      scoring = Scoring { matchScore = 1, mismatchPenalty = 2, gapPenalty = 1 }
      n = length sequence1
      m = length sequence2
      indexes = diagonalIndices n


      calculateScore :: (Int, Int) -> Int
      calculateScore (i, j)
        | i == 0    = -j * gapPenalty scoring
        | j == 0    = -i * gapPenalty scoring
        | otherwise = scores ! (i-1, j-1)

      scores :: Array (Int, Int) Int
      scores = listArray ((0, 0), (n, m)) $ parMap rpar calculateScore (diagonalIndices (n))

  
  print indexes
  printScores scores
  putStrLn $ "Results have been returned"
