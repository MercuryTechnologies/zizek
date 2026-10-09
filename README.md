> [!CAUTION]
> This project started off as a bit of an experiment in seeing whether I could tolerate using LLMs for programming tasks (with somewhat mixed results); to that end, there is still a fair amount of slop that needs cleaning up so please mind the dust.
>
> You should use this library with care and report anything that seems confusing or incorrect (especially so if it's in the form of an overly prescriptive, sycophantic comment in the code).

> [!NOTE]
> This is not an official Mercury Technologies product.

# Hegel for Haskell

> "I think that the task of philosophy is not to provide answers, but to show how the way we perceive a problem can be itself part of a problem."
>
> Slavoj Žižek[^1]

`zizek` is a property-based testing library for Haskell; it is based on [Hypothesis] and uses the [Hegel protocol] to expose Hypothesis' [library of high-quality generation strategies](https://hypothesis.readthedocs.io/en/latest/reference/strategies.html) as well as its [integrated shrinking functionality](https://hypothesis.works/articles/integrated-shrinking/).

Should we ever produce an Antithesis SDK for Haskell[^2], tests written with `zizek` will be able to integrate with it and receive more intelligent state-space exploration and increased bug-finding power for free.

[Hypothesis]: https://github.com/hypothesisworks/hypothesis
[Hegel protocol]: https://hegel.dev/reference/protocol

[^1]: [It's a philosophy joke.](https://antithesis.com/blog/2026/hegel/)
[^2]: See [antithesishq/antithesis-sdk-rust](https://github.com/antithesishq/antithesis-sdk-rust) for reference

## Contents

- [How It Works](#how-it-works)
- [Installation](#installation)
- [Usage](#usage)
  - [Simple Generators](#simple-generators)
  - [Independent Generators](#independent-generators)
  - [Dependent Generators](#dependent-generators)
  - [Collection Generators](#collection-generators)
  - [Recursive Generators](#recursive-generators)
  - [Stateful Testing](#stateful-testing)
  - [Event Statistics](#event-statistics)
  - [Integrations](#integrations)
  - [Settings & Profiles](#settings--profiles)
- [Generators](#generators)
- [Development](#development)
- [Frequently Asked Questions](#frequently-asked-questions)

## How It Works

`zizek` does not implement its own generators or shrinking logic. Instead, this library provides combinators for building generation strategies that draw from the in-process `libhegel` engine via FFI. Everything related to random sampling, choice sequence bookkeeping, and integrated shrinking happens inside `libhegel` and is communicated back to `zizek` via the [Hegel protocol].

Every primitive is a single typed FFI call. `Gen.bool`, `Gen.int & Gen.min 0 & Gen.max 100`, and `Gen.text` each draw one value from the engine this way. `zizek` builds compounds on top of these draws entirely on the client side. A tuple, `oneOf`, `frequency`, `filtered`, or a generator built with `>>=` wraps its underlying draws in labelled spans, and the engine shrinks each span as a unit. Lists, sets, and maps work the same way, but also drive the engine's collection primitive to handle their variable length.

> [!IMPORTANT]
> Complex generators can (and should!) be constructed using `do`-notation and the `ApplicativeDo` language extension; this allows `zizek` to infer dependency relationships between draws and produce an optimal generation strategy with relatively little effort.

## Installation

> [!IMPORTANT]
> `zizek` links the `libhegel` C library via pkg-config (`pkgconfig-depends: hegel` in `zizek.cabal`). It is provided by this project's Nix dev shell; downstream consumers will need `libhegel` discoverable by pkg-config on their system.

This project is not yet published to Hackage and has dependencies that are, themselves, not yet published to Hackage.

To include `zizek` in your project, make the following additions to your `cabal.project` file and add `zizek` as a package dependency to your library itself:

<details> <summary>cabal.project fragment</summary>

```
source-repository-package
  type: git
  location: https://github.com/MercuryTechnologies/zizek
  tag: main
```

</details>

## Usage

`zizek` tries to wrap the underlying `hegel` machinery in a higher-level API for constructing and exercising complex generators.

### Simple Generators

For example, consider the following property that generates machine integers in the range `[0,1000]` and then validates trivial property that `n + 1 > n` for all of these values:

```haskell
import Data.Function ((&))
import Hegel (assert, prop)
import Hegel.Gen qualified as Gen

prop_successor :: IO ()
prop_successor = do
  let ints = Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build
  prop ints \n ->
    assert (n + 1 > n) "successor should be greater"
```

### Independent Generators

When the draws in a `do` block don't reference each other, and `ApplicativeDo` has been enabled, `zizek` will group them as siblings under a single span so the engine can shrink each component independently when it finds a counterexample.

```haskell
{-# LANGUAGE ApplicativeDo #-}

import Data.Function ((&))
import Hegel (Gen, assert, prop)
import Hegel.Gen qualified as Gen

boolAndInt :: Gen (Bool, Int)
boolAndInt = do
  b <- Gen.bool                           & Gen.build
  n <- Gen.int & Gen.min 0 & Gen.max 100  & Gen.build
  pure (b, n)

prop_pair :: IO ()
prop_pair = prop boolAndInt \(_, n) ->
  assert (n >= 0 && n <= 100) "second component out of range"
```

> [!NOTE]
> When the above example is compiled and run without `ApplicativeDo`, each draw is sequenced with `>>=`, which forces `zizek` to treat `n` as a draw that depends on `b`.

### Dependent Generators

When a later draw needs to look at an earlier one, each must be sequenced as part of its own request. This lets us establish dependent relationships between generated values.

The `Hegel.Property` monad is the natural home for this: a property interleaves draws (`forAll`), effects, and assertions, and the engine shrinks across the whole interleaving. Each `forAll` is its own request, so a later draw can constrain itself with an earlier value, and `annotate` attaches context that shows up in the failure report:

```haskell
import Data.Function ((&))
import Hegel (annotate, assert, check_, def, forAll)
import Hegel.Gen qualified as Gen

prop_intervalOrdered :: IO ()
prop_intervalOrdered = check_ def do
  lo <- forAll (Gen.int & Gen.min 0  & Gen.max 100 & Gen.build)
  hi <- forAll (Gen.int & Gen.min lo & Gen.max 100 & Gen.build)
  annotate "interval should be ordered"
  assert (lo <= hi) "interval invariant broken"
```

Alternatively, when the dependency is a precondition rather than a constraint you can express directly, `Gen.assume` states it inline; if the condition fails, the test case is discarded rather than counted as a failure:

```haskell
interval :: Gen (Int, Int)
interval = do
  lo <- Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
  hi <- Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
  Gen.assume (lo < hi)
  pure (lo, hi)
```

> [!TIP]
> Unlike `Gen.filtered`, `Gen.assume` does not retry; discarded cases accumulate in `Stats.invalid` and if the predicate rejects too often, the runner reports a `GaveUp` outcome.

### Collection Generators

`zizek` also supports generation of collections types, each of which accept an element generator and expose additional parameters for shaping the collection: `minSize`/`maxSize` set bounds and, for lists, `Gen.unique` accepts a predicate function to discriminate based on equality.

```haskell
import Data.Function ((&))
import Data.List (nub)
import Hegel (Gen, prop, (===))
import Hegel.Gen qualified as Gen

uniqueInts :: Gen [Int]
uniqueInts =
  let items = Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build
  in
    Gen.list items
      & Gen.minSize 1
      & Gen.maxSize 10
      & Gen.unique (==)
      & Gen.build

prop_uniqueInts :: IO ()
prop_uniqueInts = prop uniqueInts \xs ->
  length xs === length (nub xs)
```

### Recursive Generators

Self-referential generators must wrap each recursive edge in `Gen.defer`; failing to do so will very likely result in `<<loop>>` exceptions when the generator is constructed.

```haskell
import Data.Function ((&))
import Hegel (Gen, prop, (===))
import Hegel.Gen qualified as Gen

data Tree = Leaf Int | Branch Tree Tree
  deriving stock Show

tree :: Gen Tree
tree = Gen.oneOf [leaf, branch]
  where
    leaf   = Leaf <$> (Gen.int & Gen.min 0 & Gen.max 10 & Gen.build)
    branch = Branch <$> Gen.defer tree <*> Gen.defer tree

prop_tree :: IO ()
prop_tree = prop tree \t ->
  leaves t === branches t + 1
  where
    leaves (Leaf _)     = 1
    leaves (Branch l r) = leaves l + leaves r
    branches (Leaf _)     = 0
    branches (Branch l r) = 1 + branches l + branches r
```

> [!TIP]
> Prefer `Gen.recursive` for complex recursive types; this gives `libhegel` control over how deep the structure grows and allows it to shrink a value down to one of its subtrees:
>
> ```haskell
> tree :: Gen Tree
> tree =
>   Gen.recursive leaf (\_ subtree -> Branch <$> subtree <*> subtree)
>     & Gen.maxDepth 6
>     & Gen.build
>   where
>     leaf = Leaf <$> (Gen.int & Gen.min 0 & Gen.max 10 & Gen.build)
> ```

### Stateful Testing

A stateful test is a `Machine` consisting of:
- an initial state
- the `Rule`s that can act on it
- the `Invariant`s that should hold between steps.

`libhegel` chooses which rules to run and in what order, and when a run fails, it shrinks the steps down to the shortest sequence that still fails.

The machine below checks a FIFO queue against a list, and the queue has a bug:

```haskell
import Data.Function ((&))
import Hegel (check_, def, discard, forAllWithLabel, (===))
import Hegel.Gen qualified as Gen
import Hegel.Stateful (Machine (..))
import Hegel.Stateful qualified as Stateful

-- A FIFO queue kept as two lists: pushes go onto the back list, and pops come
-- off the front list; the front list is refilled from the back when it runs out.
data Queue = Queue [Int] [Int]

push :: Int -> Queue -> Queue
push x (Queue front back) = Queue front (x : back)

pop :: Queue -> (Maybe Int, Queue)
pop (Queue (x : front) back) = (Just x, Queue front back)
pop (Queue [] []) = (Nothing, Queue [] [])
pop (Queue [] back) = pop (Queue back []) -- BUG: should be `reverse back`

-- The queue under test, paired with a list of what it should contain.
data State = State Queue [Int]

pushRule :: Stateful.Rule State IO
pushRule = Stateful.rule "push" \(State queue model) -> do
  x <- forAllWithLabel "x" $ Gen.int & Gen.build
  pure $ State (push x queue) (model ++ [x])

popRule :: Stateful.Rule State IO
popRule = Stateful.rule "pop" \(State queue model) -> case model of
  [] -> discard
  expected : rest -> do
    let (actual, queue') = pop queue
    actual === Just expected
    pure $ State queue' rest

prop_queue :: IO ()
prop_queue = check_ def $ Stateful.run Machine
  { initial    = pure $ State (Queue [] []) []
  , rules      = [pushRule, popRule]
  , invariants = []
  , stepCount  = Stateful.defaultStepCount
  }
```

Running `prop_queue` finds the bug and shrinks it down to two pushes and a pop:

```
failed after 561 tests, including shrinking
  Step 1: push
    Draw 1: x=0
  Step 2: push
    Draw 1: x=1
  Step 3: pop
    ✗ === failed, values are not equal
        (- lhs) (+ rhs)
        - Just 1
        + Just 0
```

> [!NOTE]
> A rule that calls `discard` is skipped for that step, so `popRule` only runs when the model has something to pop.

### Event Statistics

`zizek` provides two functions for collecting statistics from property runs:
- `event`, which reports the share of test cases that recorded the given label
- `eventValue`, which reports the distribution of the numeric values it's provided under the given label

```haskell
import Data.Function ((&))
import Hegel (Settings (..), check_, def, event, eventValue, forAll, (===))
import Hegel.Gen qualified as Gen

prop_reverse :: IO ()
prop_reverse = check_ def {showStatistics = Just True} do
  xs <- forAll $ Gen.list (Gen.int & Gen.build) & Gen.build
  event $ if null xs then "empty" else "non-empty"
  eventValue "length" $ fromIntegral (length xs)
  reverse (reverse xs) === xs
```

...which will produce a report like the following:

```
Statistics (over 100 test cases):
  * empty: 7.0% of test cases
  * non-empty: 93.0% of test cases
  * length: count 100, min 0, median 4, mean 5.43, p90 10, max 23
```

> [!NOTE]
> The `showStatistics` configuration option must be `True` to get a statistics report after a property run.

### Integrations

`zizek` ships adapters for the common test runners, so a `Property` can be a leaf in an existing suite.

#### `tasty`

With `tasty`, `Hegel.Tasty.testProperty` turns a property into a `TestTree`, keyed for replay by the test name (use `testPropertyWith` for custom `Settings`):

```haskell
import Data.Function ((&))
import Hegel (forAll, (===))
import Hegel.Gen qualified as Gen
import Hegel.Tasty (testProperty)
import Test.Tasty (TestTree)

test_reverseInvolutive :: TestTree
test_reverseInvolutive = testProperty "reverse is involutive" do
  xs <- forAll (Gen.list (Gen.int & Gen.build) & Gen.build)
  reverse (reverse xs) === xs
```

#### `hspec`

With `hspec`, `Hegel.Hspec.prop` is a drop-in for `it` that runs a property and persists any failure under a key derived from the test's path, so a counterexample replays on the next run:

```haskell
import Data.Function ((&))
import Hegel (forAll, (===))
import Hegel.Gen qualified as Gen
import Hegel.Hspec (prop)
import Test.Hspec

spec :: Spec
spec = describe "reverse" do
  prop "is involutive" do
    xs <- forAll (Gen.list (Gen.int & Gen.build) & Gen.build)
    reverse (reverse xs) === xs
```

> [!TIP]
> Use `propWith` to supply `Settings`; for example, `propWith def {database = Just DatabaseDisabled}` disables the replay database for the property.

For properties written over a monad-transformer stack, `propT` takes a function that can evaluate the transformer down to the `IO` context that the engine runs in (`forall x. m x -> IO x`).

For a `ReaderT` stack that runner is just `runReaderT` applied to the environment:

```haskell
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Function ((&))
import Hegel (assert, forAll)
import Hegel.Gen qualified as Gen
import Hegel.Hspec (propT)
import Test.Hspec

-- A property over a `ReaderT` stack; in practice the environment is typically
-- some slice of an application's context.
spec :: Spec
spec = describe "trivial reader example" do
  propT (\_ m -> runReaderT m 100) "stays within the bound" do
    n     <- forAll (Gen.int & Gen.min 0 & Gen.max 100 & Gen.build)
    bound <- lift ask
    assert (n <= bound) "stays within the configured bound"
```

> [!TIP]
> Use `propWithT` to supply custom `Settings`.

### Settings & Profiles

Every run starts from a profile, a named set of defaults; `def` runs the profile as-is, and `def {testCases = Just 500}` runs it with 500 test cases.

`libhegel` picks one of three built-in profiles based on where the tests run:
- `development` for local runs, which stores failures under `.hegel/` so they replay on the next run
- `ci` on a CI server, which makes runs deterministic and stores nothing
- `workload` inside Antithesis

A `hegel.toml` file in the test's working directory, or any directory above it, adjusts these profiles and can defines new ones:

```toml
# Run more cases on CI.
[profiles.ci]
test_cases = 1000

# A profile for longer runs, layered over whichever profile the environment picks.
[profiles.nightly]
test_cases = 10000
```

A property can select the profile it uses by overriding its settings `def {profile = Just "nightly"}`, `HEGEL_DEFAULT_PROFILE=nightly` can be used to set a default property for an entire run, and `Hegel.Profile.register` provides a more advanced API for defining and setting a profile in Haskell code.

## Generators

| Builder                    | Produces               | Modifiers                                                                                                            |
| -------------------------- | ---------------------- | -------------------------------------------------------------------------------------------------------------------- |
| `bool`                     | `Bool`                 | `weighted`                                                                                                           |
| `integral`                 | any `Integral a`       | `min`, `max`                                                                                                         |
| `int`, `int8`…`int64`      | signed ints            | `min`, `max`                                                                                                         |
| `word`, `word8`…`word64`   | unsigned ints          | `min`, `max`                                                                                                         |
| `float`, `double`          | floating point numbers | `min`, `max`, `exclusiveMin`, `exclusiveMax`, `disallowNan`, `disallowInfinity`                                      |
| `binary`                   | `ByteString`           | `minSize`, `maxSize`                                                                                                 |
| `text`                     | `Text`                 | `minSize`, `maxSize`, `alphabet`                                                                                     |
| `char`                     | `Char`                 | `codec`, `minCodepoint`, `maxCodepoint`, `categories`, `excludeCategories`, `includeCharacters`, `excludeCharacters` |
| `uuid`                     | `UUID`                 | `version`                                                                                                            |
| `uri`, `uriText`           | parsed / raw URIs      | —                                                                                                                    |
| `date`                     | `Day`                  | `min`, `max`, `minYear`, `maxYear`                                                                                   |
| `time`                     | `TimeOfDay`            | `min`, `max`                                                                                                         |
| `datetime`                 | `LocalTime`            | `min`, `max`, `minYear`, `maxYear`, `onDay`                                                                          |
| `duration`                 | `NominalDiffTime`      | `min`, `max`                                                                                                         |
| `domain`                   | domain names           | `maxLength`                                                                                                          |
| `email`                    | email addresses        | —                                                                                                                    |
| `regex`                    | strings matching regex | `fullMatch`, `alphabet`                                                                                              |
| `list`                     | linked lists           | `minSize`, `maxSize`, `unique`                                                                                       |
| `nonEmpty`                 | `NonEmpty a`           | `minSize`, `maxSize`                                                                                                 |
| `recursive`                | self-referential data  | `maxDepth`, `maxLeaves`                                                                                              |
| `set`, `hashSet`, `intSet` | set variants           | `minSize`, `maxSize`                                                                                                 |
| `map`, `hashMap`, `intMap` | map variants           | `minSize`, `maxSize`                                                                                                 |

| Combinator                                     | Purpose                                                                       |
| ---------------------------------------------- | ------------------------------------------------------------------------------|
| `oneOf :: [Gen a] -> Gen a`                    | Choose from a list of generators; the list must be non-empty                  |
| `element :: [a] -> Gen a`                      | Choose from a list of values; the list must be non-empty                      |
| `enum :: Enum a => a -> a -> Gen a`            | A value between two bounds, inclusive                                         |
| `enumBounded :: (Bounded a, Enum a) => Gen a`  | Any value of a bounded enumeration                                            |
| `frequency :: [(Int, Gen a)] -> Gen a`         | Weighted choice; all weights must be positive                                 |
| `maybe :: Gen a -> Gen (Maybe a)`              | `Nothing` or `Just` a generated value                                         |
| `either :: Gen a -> Gen b -> Gen (Either a b)` | `Left` from the first generator or `Right` from the second                    |
| `assume :: Bool -> Gen ()`                     | Conditionally discard the current test case                                   |
| `discard :: Gen a`                             | Unconditionally discard the current test case                                 |
| `defer :: Gen a -> Gen a`                      | Break recursion cycles in self-referential generators                         |
| `filtered :: (a -> Bool) -> Gen a -> Gen a`    | Draw until a value satisfies the predicate, discarding after 3 attempts       |
| `mapMaybe :: (a -> Maybe b) -> Gen a -> Gen b` | Draw until the function produces `Just b`, discarding after 3 attempts        |
| `just :: Gen (Maybe a) -> Gen a`               | Draw until the generator produces `Just a`, discarding after 3 attempts       |

> [!NOTE]
> `oneOf`, `element`, and `frequency` do not sample uniformly, or even in proportion to their weights; `libhegel` biases towards choices that lead to inputs it hasn't seen yet, so over a run it tries to draw more from branches that can still produce new values.
>
> For example, `oneOf [Gen.bool, Gen.int32]` runs out of new `Bool`s early on and spends the rest of its run generating `Int32`s; similarly, `frequency`'s weights bias which branch the engine will select initially but **will not necessarily** produce a distribution that reflects these weights.

## Development

Clone the repository and enter the development shell with `nix develop`.

Common development actions can performed with the `just` command runner, for example:

```shell
$ just check             # CI checks: check-format + build + test
$ just build             # compile the library & test suite
$ just test              # run the unit test suite
$ just test <name>       # run a specific test suite
$ just format            # run all formatters
$ just check-format      # verify formatting without modifying files
$ just docs              # build Haddocks
$ just repl              # start a GHCi session with this library in-scope
```

## Frequently Asked Questions

### What's missing?

Some work remains outstanding:

* an API to seed the `Explicit` phase with hand-written examples
* Hackage publication

### What's up with the name?

Slavoj Žižek is a contemporary philosopher described as "Hegelo-Lacanian", and whose work deals largely with the implications of how so much of human behavior is rooted in 'ideology'.

Haskell programmers are often characterized as having an excessive (one might say _ideological_) fixation with correctness, which often finds itself at odds with the tools that are associated with more pragmatically-minded folk.

So it seems appropriate that `zizek` is the mechanism by which we interface with `hegel`.

### ...what?

> "Even Lacan is just a tool for me to read Hegel. For me, always it is Hegel, Hegel, Hegel."
>
> Slavoj Žižek

## Acknowledgements

[Antithesis](https://antithesis.com/) for producing [`hegel-core`](https://github.com/hegeldev/hegel-core), [`hegel-rust`](https://github.com/hegeldev/hegel-rust), and the other Hegel libraries that were used as references during the development of `zizek`.

[`hedgehog`](https://hackage.haskell.org/package/hedgehog), for acting as a fantastic reference API property-based testing in Haskell.

[`QuickCheck`](https://hackage.haskell.org/package/QuickCheck), for being the first property-based testing library I ever used and making it difficult to imagine building software without something like it.

[Mercury Technologies](https://mercury.com/), for providing a supportive environment that allowed this project to develop in the course of one of our company hack weeks (if this sounds interesting to you, [we're hiring!](https://mercury.com/jobs))
