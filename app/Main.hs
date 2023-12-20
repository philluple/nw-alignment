{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Lib
import System.Environment (getArgs, getProgName)
import Debug.Trace


calculateDScore :: String -> String -> [[Int]]
calculateDScore s1 s2 =
  let n = length s1
      m = length s2
      initialDiagonals = [[0], [-1, -1]]  -- Initialize first two diagonals
  in foldl (updateDiagonals s1 s2 n m) initialDiagonals [2..n+m]


updateDiagonals :: String -> String -> Int -> Int -> [[Int]] -> Int -> [[Int]]
updateDiagonals s1 s2 n m previousDiagonals i =
  trace ("i: " ++ show i ++ ", head: " ++ show (head previousDiagonals) ++ ", tail: " ++ show (head (tail previousDiagonals))) $
    let newDiagonal = calculateScore s1 s2 (head previousDiagonals) (head (tail previousDiagonals)) i n m
    in [(head (tail previousDiagonals)), newDiagonal]  -- Keep only the last two diagonals


calculateScore :: String -> String -> [Int] -> [Int] -> Int -> Int -> Int -> [Int]
calculateScore s1 s2 prevDiag1 prevDiag2 counter n m =
  let indices = if counter <= m then [0..counter] else [counter - m..n]
  in map (calculateValue s1 s2 prevDiag1 prevDiag2 counter n m) indices


calculateValue :: String -> String -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int
calculateValue s1 s2 prevDiag1 prevDiag2 counter n m i =
  let inBounds1 = i > 0 && i <= n
      inBounds2 = counter - i > 0 && counter - i <= m
      matchOrMismatch = if inBounds1 && inBounds2 && (s1 !! (i - 1)) == (s2 !! (counter - i-1)) 
                        then 1 else -1
      scoreDiag = if i > 0 && i - 1 < length prevDiag2 then (prevDiag2 !! (i - 1)) + matchOrMismatch else -1
      scoreUp = if i < length prevDiag1 then (prevDiag1 !! i) - 1 else -1
      scoreLeft = if i > 0 && i - 1 < length prevDiag1 then (prevDiag1 !! (i - 1)) - 1 else -1
  in maximum [scoreDiag, scoreUp, scoreLeft]

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
          let (alignd1, alignd2) = traceback matrix seq1 seq2
          putStrLn "Alignment is done! Writing aligned sequences to files"
          let outputFile = "./output/output.txt"
          writeFile outputFile $ unlines [alignd1, alignd2]
        "row" -> do
          let matrix = row schema seq1 seq2
              (alignd1, alignd2) = traceback matrix seq1 seq2
          putStrLn "Alignment is done! Writing aligned sequences to files"
          let outputFile = "./output/output.txt"
          writeFile outputFile $ unlines [alignd1, alignd2]
        "column" -> do
          let matrix = column schema seq1 seq2
              (alignd1, alignd2) = traceback matrix seq1 seq2
          putStrLn "Alignment is done! Writing aligned sequences to files"
          let outputFile = "./output/output.txt"
          writeFile outputFile $ unlines [alignd1, alignd2]
        "sequential" -> do
          let matrix = sequential schema seq1 seq2
              (alignd1, alignd2) = traceback matrix seq1 seq2
          putStrLn "Alignment is done! Writing aligned sequences to files"
          let outputFile = "./output/output.txt"
          writeFile outputFile $ unlines [alignd1, alignd2]
        _ -> do
          putStrLn "Invalid method specified"
          return ()
    _ -> do 
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " <method> <filename>"
