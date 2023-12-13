module Main where

import Control.Exception
import Control.Monad
import Lib
import Test.HUnit.Base
import Test.HUnit.Text (runTestTT)

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
    _ <- action
    assertFailure $ "Expected exception: " ++ show ex
  where
    isWanted = guard . (== ex)

tests :: Test