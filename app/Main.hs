import Lib
import System.Environment (getArgs)


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

  let sequence1 = seq1
      sequence2 = seq2
      scoring = Scoring { matchScore = 1, mismatchPenalty = 2, gapPenalty = 1 }
      scores = needlemanWunsch scoring sequence1 sequence2
      (alignment1, alignment2) = traceback scores sequence1 sequence2
      outputFileSequence1 = "./output/output1.txt"
      outputFileSequence2 = "./output/output2.txt"

  writeFile outputFileSequence1 alignment1
  writeFile outputFileSequence2 alignment2
  putStrLn $ "Results have been returned"
