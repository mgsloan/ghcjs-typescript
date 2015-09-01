module Main where

import System.Process
import System.Directory (doesFileExist)
import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  (_, actual1, actual2) <- readProcessWithExitCode "stack" ["exec", "--", "ghcjs", "-i", "-itest", "test/input.hs"] ""
  let actual = actual1 ++ "\n" ++ actual2
  writeFile "test/actual-output" actual
  let expectedPath = "test/expected-output"
  exists <- doesFileExist expectedPath
  if exists
    then do
      expected <- readFile expectedPath
      when (expected /= actual) $ do
        rawSystem "diff" ["--unified", "test/expected-output", "test/actual-output"]
        exitFailure
      exitSuccess
    else do
      putStrLn "No file at test/expected-output"
      exitFailure
