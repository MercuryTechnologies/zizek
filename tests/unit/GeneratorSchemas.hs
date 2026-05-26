module GeneratorSchemas (generatorSchemasTests) where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Hegel (runProperty_)
import Hegel.Generators (Generator, assume, filtered, oneOf)
import Hegel.Generators.Integer qualified as Integer
import Hegel.Runner (defaultSettings)

intR :: (Int, Int) -> Generator Int
intR r = Integer.gen $ Integer.integers @Int & Integer.withRange r

generatorSchemasTests :: IO ()
generatorSchemasTests = do
  pureTest
  apBasicTest
  singleLeafApTest
  mapFusionTest
  oneOfBasicTest
  nestedApOneOfTest
  bindFallbackTest
  filteredTest
  assumeTest

-- Pure a — exercises unitSchema {"type":"constant","value":null}
pureTest :: IO ()
pureTest = do
  putStrLn "Running pureTest..."
  runProperty_ defaultSettings (pure (42 :: Int)) $ \n ->
    if n == 42
      then pure ()
      else error ("pureTest: expected 42, got " <> show n)
  putStrLn "PASSED"

-- Ap of two basics — exercises tupleSchema
apBasicTest :: IO ()
apBasicTest = do
  putStrLn "Running apBasicTest..."
  let g = (,) <$> intR (0, 10) <*> intR (0, 10)
  runProperty_ defaultSettings g $ \(a, b) ->
    if a >= 0 && a <= 10 && b >= 0 && b <= 10
      then pure ()
      else error ("apBasicTest: out of range: " <> show (a, b))
  putStrLn "PASSED"

-- Ap (Pure f) ga — single-leaf optimisation (no TUPLE span, no tuple schema)
singleLeafApTest :: IO ()
singleLeafApTest = do
  putStrLn "Running singleLeafApTest..."
  let g = pure (+ 1) <*> intR (0, 10)
  runProperty_ defaultSettings g $ \n ->
    if n >= 1 && n <= 11
      then pure ()
      else error ("singleLeafApTest: out of range: " <> show n)
  putStrLn "PASSED"

-- fmap fusion: fmap f (Map g x) = Map (f . g) x
mapFusionTest :: IO ()
mapFusionTest = do
  putStrLn "Running mapFusionTest..."
  let g = fmap (+ 1) (fmap (* 2) (intR (0, 10)))
  runProperty_ defaultSettings g $ \n ->
    if n >= 1 && n <= 21 && odd n
      then pure ()
      else error ("mapFusionTest: unexpected value: " <> show n)
  putStrLn "PASSED"

-- OneOf of all-basic generators — exercises oneOfSchema
oneOfBasicTest :: IO ()
oneOfBasicTest = do
  putStrLn "Running oneOfBasicTest..."
  let g = oneOf (intR (0, 10) :| [intR (20, 30)])
  runProperty_ defaultSettings g $ \n ->
    if (n >= 0 && n <= 10) || (n >= 20 && n <= 30)
      then pure ()
      else error ("oneOfBasicTest: out of range: " <> show n)
  putStrLn "PASSED"

-- Nested Ap + OneOf — schema nesting
nestedApOneOfTest :: IO ()
nestedApOneOfTest = do
  putStrLn "Running nestedApOneOfTest..."
  let g = (,) <$> oneOf (intR (0, 5) :| [intR (10, 15)]) <*> intR (0, 10)
  runProperty_ defaultSettings g $ \(a, b) ->
    if ((a >= 0 && a <= 5) || (a >= 10 && a <= 15)) && b >= 0 && b <= 10
      then pure ()
      else error ("nestedApOneOfTest: out of range: " <> show (a, b))
  putStrLn "PASSED"

-- Bind (monadic) fallback — exercises FLAT_MAP span
bindFallbackTest :: IO ()
bindFallbackTest = do
  putStrLn "Running bindFallbackTest..."
  let g = intR (0, 5) >>= \lo -> intR (lo, lo + 5)
  runProperty_ defaultSettings g $ \n ->
    if n >= 0 && n <= 10
      then pure ()
      else error ("bindFallbackTest: out of range: " <> show n)
  putStrLn "PASSED"

-- filtered — exercises FILTER span and retry logic
filteredTest :: IO ()
filteredTest = do
  putStrLn "Running filteredTest..."
  let g = filtered even (intR (0, 20))
  runProperty_ defaultSettings g $ \n ->
    if even n
      then pure ()
      else error ("filteredTest: expected even, got " <> show n)
  putStrLn "PASSED"

-- assume — discards test cases, run must not error
assumeTest :: IO ()
assumeTest = do
  putStrLn "Running assumeTest..."
  let g = do
        n <- intR (0, 20)
        assume (n /= 7)
        pure n
  runProperty_ defaultSettings g $ \n ->
    if n /= 7
      then pure ()
      else error ("assumeTest: got 7, should have been discarded")
  putStrLn "PASSED"
