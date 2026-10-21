# Generators

The examples assume the `BlockArguments`, `OverloadedStrings`, `OverloadedRecordDot`, `NamedFieldPuns`, and `DuplicateRecordFields` extensions.

## Builders

Leaf generators are builders. Chain modifiers onto them with `&` and finish with `Gen.build`. Type-pinned aliases such as `Gen.int`, `Gen.int64`, and `Gen.word8` fix the element type.

```haskell
import Data.Function ((&))
import Hegel.Gen qualified as Gen

small = Gen.int & Gen.min 0 & Gen.max 7 & Gen.build
names = Gen.text & Gen.minSize 1 & Gen.maxSize 16 & Gen.build
keys  = Gen.list small & Gen.unique (==) & Gen.maxSize 8 & Gen.build
```

Choice combinators such as `Gen.oneOf`, `Gen.element`, `Gen.frequency`, `Gen.enumBounded`, and `Gen.permutation` produce a `Gen` directly, with no `Gen.build`. A modifier that doesn't apply to a builder is a type error.

## Dependent and independent draws

Draw a value that depends on another from it, with `>>=` in a generator or sequential `forAll`s in a property.

```haskell
withIndex :: Gen ([Int], Int)
withIndex = do
  xs <- Gen.list small & Gen.minSize 1 & Gen.build
  i <- Gen.int & Gen.min 0 & Gen.max (length xs - 1) & Gen.build
  pure (xs, i)
```

Enable `ApplicativeDo` so do-blocks whose draws don't depend on each other become applicative. The engine can then shrink each independent draw on its own.

## Filtering

`Gen.filtered` and `Gen.mapMaybe` make up to three attempts before discarding the test case, and `assume` discards it at once. An occasional discard is cheap. A generator that discards most of what it draws spends the run on nothing and fails the `FilterTooMuch` health check, so construct the value instead.

## Default bounds

- **Integers** default to the type's full range, and `Gen.min` and `Gen.max` are inclusive.
- **Floats** produce NaN and both infinities by default. Setting `Gen.min` or `Gen.max` excludes NaN, and setting both excludes infinity. `Gen.disallowNan` and `Gen.disallowInfinity` exclude them unconditionally, and `Gen.exclusiveMin` and `Gen.exclusiveMax` make a bound strict.
- **Sizes** of text, binary, and lists default to a minimum of zero and no maximum. Set them with `Gen.minSize` and `Gen.maxSize`.

The engine already favors boundary values, but only within the bounds you set, so leave bounds wide wherever the code under test should handle the whole range.

## Text

Pass a character builder to `Gen.alphabet` to control which characters text contains. `Hegel.Alphabet` provides presets for awkward input, including `combiningMarks`, `zeroWidth`, `bidiControls`, and `whitespace`.

```haskell
import Hegel.Alphabet qualified as Alphabet

marks = Gen.text & Gen.alphabet Alphabet.combiningMarks & Gen.build
```

`Gen.regex` produces text containing a match for its pattern, and `Gen.fullMatch` requires the whole text to match. Without `Gen.fullMatch` the text may carry arbitrary characters around the match, which makes near-valid input easy to get.

## Choosing among alternatives

`Gen.oneOf` and `Gen.element` pick each alternative about equally often, and `Gen.frequency` picks in proportion to its weights. A counterexample shrinks toward the first alternative in the list, so put the simplest one first. When crossing input classes deliberately, draw the class itself as a small type with `Gen.enumBounded` or `Gen.element` and construct the case from it, so each case records which class it belongs to.

## Recursive data

`Gen.recursive` takes a leaf generator and a function that builds a branch from a generator of sub-values. The engine bounds depth with `Gen.maxDepth`, which defaults to 32, and total leaves with `Gen.maxLeaves`, which defaults to 100. Control breadth through the size of the list each branch draws.

```haskell
data Tree = Leaf Int | Branch [Tree]
  deriving stock (Show)

trees :: Gen Tree
trees =
  Gen.recursive
    (Leaf <$> (Gen.int & Gen.build))
    (\_ctx subtrees -> Branch <$> (Gen.list subtrees & Gen.maxSize 3 & Gen.build))
    & Gen.maxDepth 4
    & Gen.maxLeaves 20
    & Gen.build
```

The branch function's first argument, a `RecursionContext`, gives the current `depth` and the configured `maxDepth` for branches that should change shape as they nest.

## Values from a fixed list

- `Gen.permutation xs` reorders a list and shrinks toward the original order.
- `Gen.subsequence xs` picks an order-preserving subset, sized with `Gen.minSize` and `Gen.maxSize`.
- `Gen.sample xs` draws a list of values from `xs`, repeating values by default. Add `Gen.withoutReplacement` to take each position at most once.

## Looking at a generator's output

`Hegel.samples def 20 gen` returns twenty values from `gen` in a REPL. They come from the same distribution a property draws from, edge cases included, so they show what a generator actually produces. Never call it from inside a property: it starts a separate engine run, its values never enter the test case, and replays then look nondeterministic. `Hegel.samples` is unrelated to the `Gen.sample` builder.

## Targeting

`target score` steers the engine toward test cases with higher scores, and `targetWithLabel label score` tracks several independent scores. Negate a score to minimize it. Scores must be finite.

Each label may be recorded at most once per test case, counting every branch, fork, and stateful worker, and a repeat aborts the run. To target a per-step quantity in a stateful machine, keep its running maximum in an `IORef` and call `target` once after `Hegel.Stateful.run` returns.

Targeting runs by default. Runs of 1,000 test cases or more begin targeting after a short warm-up and alternate it with random generation, while smaller runs get a single targeting pass halfway through. Raise `testCases` for a run that relies on targeting to reach a rare state.
