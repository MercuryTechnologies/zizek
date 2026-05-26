module Main (main) where

import Control.Exception (finally)
import Data.Function ((&))
import GeneratorSchemas (generatorSchemasTests)
import Hegel (Phase (..), closeSession, runProperty, runProperty_)
import Hegel.Generators.Integer qualified as Integer
import Hegel.Outcome (Outcome (..))
import Hegel.Runner (Settings (..), defaultSettings)
import SessionRecovery (sessionRecoveryTest)

main :: IO ()
main =
  ( do
      passingTest
      failingTest
      limitedPhasesTest
      sessionRecoveryTest
      generatorSchemasTests
  )
    `finally` closeSession

-- All integers in [0,100] should be in [0,100].
passingTest :: IO ()
passingTest = do
  putStrLn "Running passing property..."
  runProperty_ defaultSettings (Integer.gen $ Integer.integers @Int & Integer.withRange (0, 100)) $ \n ->
    if n >= 0 && n <= 100
      then pure ()
      else error ("out of range: " <> show n)
  putStrLn "PASSED"

-- Any integer that must not equal 42 will fail; Hypothesis should shrink to 42.
failingTest :: IO ()
failingTest = do
  putStrLn "Running failing property (expect shrunk counterexample)..."
  outcome <- runProperty defaultSettings (Integer.gen $ Integer.integers @Int & Integer.withRange (0, 100)) $ \n ->
    if n /= 42
      then pure ()
      else error "found 42"
  case outcome of
    Failed {counterexample = ce} ->
      putStrLn $ "FAILED (expected): counterexample = " <> show ce
    other -> do
      putStrLn $ "Unexpected outcome: " <> show other
      error "failingTest: expected Failed outcome"

-- Confirm settings.phases is wired through: Generate-only skips reuse/shrink phases.
limitedPhasesTest :: IO ()
limitedPhasesTest = do
  putStrLn "Running limited-phases property (Generate only)..."
  runProperty_ (defaultSettings {phases = [Generate]}) (Integer.gen $ Integer.integers @Int & Integer.withRange (0, 100)) $ \n ->
    if n >= 0 && n <= 100
      then pure ()
      else error ("out of range: " <> show n)
  putStrLn "PASSED"
