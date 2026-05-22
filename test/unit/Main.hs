module Main (main) where

import Hegel (runProperty, runProperty_)
import Hegel.Generators.Integer (integers, withRange)
import Hegel.Outcome (Outcome (..))
import Hegel.Runner (defaultSettings)

main :: IO ()
main = do
  passingTest
  failingTest

-- All integers in [0,100] should be in [0,100].
passingTest :: IO ()
passingTest = do
  putStrLn "Running passing property..."
  runProperty_ defaultSettings (integers @Int `withRange` (0, 100)) $ \n ->
    if n >= 0 && n <= 100
      then pure ()
      else error ("out of range: " <> show n)
  putStrLn "PASSED"

-- Any integer that must not equal 42 will fail; Hypothesis should shrink to 42.
failingTest :: IO ()
failingTest = do
  putStrLn "Running failing property (expect shrunk counterexample)..."
  outcome <- runProperty defaultSettings (integers @Int `withRange` (0, 100)) $ \n ->
    if n /= 42
      then pure ()
      else error "found 42"
  case outcome of
    Failed ce _msg _notes -> do
      putStrLn $ "FAILED (expected): counterexample = " <> show ce
    other -> do
      putStrLn $ "Unexpected outcome: " <> show other
      error "failingTest: expected Failed outcome"
