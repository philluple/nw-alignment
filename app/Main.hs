{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Lib
import System.Environment (getArgs, getProgName)

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
