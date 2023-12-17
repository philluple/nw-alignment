import Lib
import System.Environment (getArgs, getProgName)
import Data.Array
import Control.Monad (forM_)
import Data.IORef
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

type MutableArray = IORef (Array (Int, Int) Int)

printImmutableArray :: Array (Int, Int) Int -> String -> String -> IO ()
printImmutableArray arr s1 s2 = do
  let (_, (n, m)) = bounds arr

  -- Print the first row with sequence2
  putStrLn $ "\t" ++ concatMap (\c -> c : "\t") s2

  forM_ [0..n] $ \i -> do
    -- Print the first column with sequence1
    putStr $ if i == 0 then "\t" else s1 !! (i) : "\t"

    forM_ [0..m] $ \j -> do
      putStr $ show (arr ! (i, j)) ++ "\t"
    putStrLn ""

initializeMutableArray :: Int -> IO MutableArray
initializeMutableArray n = newIORef $ listArray ((0, 0), (n-1, n-1)) $ repeat 0


updateMutableArray :: MutableArray -> (Int, Int) -> Int -> IO ()
updateMutableArray mutableArray index newValue = do
  oldArray <- readIORef mutableArray
  let newArray = oldArray // [(index, newValue)]
  writeIORef mutableArray newArray

readIndex :: MutableArray -> (Int, Int) -> IO Int
readIndex mutableArray index = do
  currentArray <- readIORef mutableArray
  return (currentArray ! index)

antidiagonalIndices :: Int -> [[(Int, Int)]]
antidiagonalIndices n =
  [ [(i, k - i) | i <- [0..k], k - i < n, k - i >= 0 && i < n] | k <- [0..2*(n-1)] ]


adiagonal :: Scoring -> String -> String -> IO MutableArray
adiagonal scoring s1 s2 = do
  let n = length s1
      m = length s2
  mutableArray <- initializeMutableArray n
  diagonals <- return $ antidiagonalIndices n

  -- Use parMap rpar for parallelization
  forM_ diagonals $ \diag -> do
    scores <- readIORef mutableArray

    -- Use parMap rpar to parallelize the calculation of new scores
    let newScores = parMap rpar (\(i, j) -> calculateScore scoring s1 s2 i j scores) diag

    -- Update the mutable array with the new scores
    forM_ (zip diag newScores) $ \((i, j), newScore) ->
      updateMutableArray mutableArray (i, j) newScore

  return mutableArray


-- Define a helper function for calculating scores in parallel
calculateScore :: Scoring -> String -> String -> Int -> Int -> Array (Int, Int) Int -> Int
calculateScore scoring s1 s2 i j scores
  | i == 0 = -j * gapPenalty scoring
  | j == 0 = -i * gapPenalty scoring
  | otherwise = maximum
                  [ scores ! (i-1, j-1) + score scoring (s1 !! (i)) (s2 !! (j))
                  , scores ! (i, j-1) - gapPenalty scoring
                  , scores ! (i-1, j) - gapPenalty scoring
                  ]

  -- Update the type signature
traceback :: MutableArray -> String -> String -> IO (String, String)
traceback mutableArray s1 s2 = do
  scores <- readIORef mutableArray
  let (n, m) = snd $ bounds scores

  let go (i, j) (align1, align2)
        | i == 0 && j == 0 = (align1, align2)
        | i == 0 =
          let newAlign1 = '-':align1
              newAlign2 = s2 !! (j-1) : align2
          in go (i, j-1) (newAlign1, newAlign2)
        | j == 0 =
          let newAlign1 = s1 !! (i-1) : align1
              newAlign2 = '-':align2
          in go (i-1, j) (newAlign1, newAlign2)
        | scores ! (i, j) == scores ! (i-1, j) - 1 =
          let newAlign1 = s1 !! (i-1) : align1
              newAlign2 = '-' : align2
          in go (i-1, j) (newAlign1, newAlign2)
        | scores ! (i, j) == scores ! (i, j-1) - 1 =
          let newAlign1 = '-':align1
              newAlign2 = s2 !! (j-1) : align2
          in go (i, j-1) (newAlign1, newAlign2)
        | otherwise =
          let newAlign1 = s1 !! (i-1) : align1
              newAlign2 = s2 !! (j-1) : align2
          in go (i-1, j-1) (newAlign1, newAlign2)

  return $ go (n, m) ("", "")



main :: IO ()
main = do 
  args <- getArgs
  case args of
    [method, filename] -> do
      contents <- readFile filename
      let [seq1, seq2] = map (' ' :) (lines contents)
          schema = Scoring {matchScore = 1, mismatchPenalty = 2, gapPenalty = 1}
      putStrLn "Starting alignment..."
      case method of
        "adiagonal" -> do
          matrix <- adiagonal schema seq1 seq2
          immutableArray <- readIORef matrix
          printImmutableArray immutableArray seq1 seq2
          (alignd1, alignd2) <- traceback matrix seq1 seq2
          putStrLn "Alignment is done! Writing aligned sequences to files"
          let outputFile = "./output/output.txt"
          writeFile outputFile $ unlines [alignd1, alignd2]
        -- "row" -> do
        --   let matrix = row schema seq1 seq2
        --       (alignd1, alignd2) = traceback matrix seq1 seq2
        --   putStrLn "Alignment is done! Writing aligned sequences to files"
        --   let outputFile = "./output/output.txt"
        --   writeFile outputFile $ unlines [alignd1, alignd2]
        -- "column" -> do
        --   let matrix = column schema seq1 seq2
        --       (alignd1, alignd2) = traceback matrix seq1 seq2
        --   putStrLn "Alignment is done! Writing aligned sequences to files"
        --   let outputFile = "./output/output.txt"
        --   writeFile outputFile $ unlines [alignd1, alignd2]
        -- "sequential" -> do
        --   let matrix = sequential schema seq1 seq2
        --       (alignd1, alignd2) = traceback matrix seq1 seq2
        --   putStrLn "Alignment is done! Writing aligned sequences to files"
        --   let outputFile = "./output/output.txt"
        --   writeFile outputFile $ unlines [alignd1, alignd2]
        _ -> do
          putStrLn "Invalid method specified"
          return ()
    _ -> do 
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " <method> <filename>"
