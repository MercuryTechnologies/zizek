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
  - [Integrations](#integrations)
- [Generators](#generators)
- [Development](#development)
- [Frequently Asked Questions](#frequently-asked-questions)

## How It Works

`zizek` does not implement its own generators or shrinking logic. Instead, this library provides combinators for building generation strategies that are handed to the in-process `libhegel` engine via FFI. Everything related to random sampling, choice sequence bookkeeping, and integrated shrinking happens inside `libhegel` and is communicated back to `zizek` via the [Hegel protocol].

A generator built with this library is a description of a *schema*: `Gen.int & Gen.min 0 & Gen.max 100 & Gen.build` describes a generation strategy for integers in `[0,100]`.

When all the draws in a property can be expressed as one schema, `zizek` makes a single FFI call into `libhegel` and gets back the full bundle of values in one round-trip; we colloquially refer to these as "independent draws".

When one draw depends on the results of an earlier draw, or uses a combinator that is not expressible as a schema (e.g. `filtered`, `defer`), `zizek` falls back to step-by-step interactive generation, wrapping the calls in labelled spans so Hypothesis can still shrink effectively.

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

source-repository-package
  type: git
  location: https://github.com/iand675/wireform-
  tag: main
  subdir: wireform-cbor

source-repository-package
  type: git
  location: https://github.com/iand675/wireform-
  tag: main
  subdir: wireform-core

source-repository-package
  type: git
  location: https://github.com/iand675/wireform-
  tag: main
  subdir: wireform-derive
```

</details>

## Usage

`zizek` tries to wrap the underlying `hegel` machinery in a higher-level API for constructing and exercising complex generators.

### Simple Generators

For example, consider the following property that generates machine integers in the range `[0,1000]` and then validates trivial property that `n + 1 > n` for all of these values:

```haskell
import Data.Function ((&))
import Hegel (prop)
import Hegel.Gen qualified as Gen
import Hegel.Property (assert)

prop_successor :: IO ()
prop_successor = do
  let ints = Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build
  prop ints \n ->
    assert (n + 1 > n) "successor should be greater"
```

### Independent Generators

When the draws in a `do` block don't reference each other, and `ApplicativeDo` has been enabled, `zizek` will batch them into a single FFI call into `libhegel` for the whole tuple and the engine can shrink each component independently when a counterexample is found:

```haskell
{-# LANGUAGE ApplicativeDo #-}

import Data.Function ((&))
import Hegel (Gen, prop)
import Hegel.Gen qualified as Gen
import Hegel.Property (assert)

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
> The example will compile and run without `ApplicativeDo`, but `zizek` will now issue two separate FFI calls and shrinking will be dependent.

### Dependent Generators

When a later draw needs to look at an earlier one, each must be sequenced as part of its own request. This lets us establish dependent relationships between generated values.

The `Hegel.Property` monad is the natural home for this: a property interleaves draws (`forAll`), effects, and assertions, and the engine shrinks across the whole interleaving. Each `forAll` is its own request, so a later draw can constrain itself with an earlier value, and `annotate` attaches context that shows up in the failure report:

```haskell
import Data.Default.Class (def)
import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Property (annotate, assert, check_, forAll)

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
import Hegel (Gen, prop)
import Hegel.Gen qualified as Gen
import Hegel.Property ((===))

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
import Hegel (Gen, prop)
import Hegel.Gen qualified as Gen
import Hegel.Property ((===))

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

> [!NOTE]
> `Gen.defer` always falls back to interactive generation; each recursive step is a separate FFI call into `libhegel`.

### Integrations

`zizek` ships adapters for the common test runners, so a `Property` can be a leaf in an existing suite.

#### `tasty`

With `tasty`, `Hegel.Tasty.testProperty` turns a property into a `TestTree`, keyed for replay by the test name (use `testPropertyWith` for custom `Settings`):

```haskell
import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Property (forAll, (===))
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
import Hegel.Gen qualified as Gen
import Hegel.Hspec (prop)
import Hegel.Property (forAll, (===))
import Test.Hspec

spec :: Spec
spec = describe "reverse" do
  prop "is involutive" do
    xs <- forAll (Gen.list (Gen.int & Gen.build) & Gen.build)
    reverse (reverse xs) === xs
```

> [!TIP]
> Use `propWith` to supply `Settings`, and `propWith def` to disable counterexample persistence.

For properties written over a monad-transformer stack, `propT` takes a function that can evaluate the transformer down to the `IO` context that the engine runs in (`forall x. m x -> IO x`).

For a `ReaderT` stack that runner is just `runReaderT` applied to the environment:

```haskell
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Function ((&))
import Hegel.Gen qualified as Gen
import Hegel.Hspec (propT)
import Hegel.Property (assert, forAll)
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

## Generators

| Builder                    | Produces               | Modifiers                                                                                                            |
| -------------------------- | ---------------------- | -------------------------------------------------------------------------------------------------------------------- |
| `bool`                     | `Bool`                 | —                                                                                                                    |
| `integral`                 | any `Integral a`       | `min`, `max`                                                                                                         |
| `int`, `int8`…`int64`      | signed ints            | `min`, `max`                                                                                                         |
| `word`, `word8`…`word64`   | unsigned ints          | `min`, `max`                                                                                                         |
| `enum`                     | enumerated values      | `min`, `max`                                                                                                         |
| `enumBounded`              | bounded enumerations   | —                                                                                                                    |
| `float`, `double`          | floating point numbers | `min`, `max`, `exclusiveMin`, `exclusiveMax`, `disallowNan`, `disallowInfinity`                                      |
| `binary`                   | `ByteString`           | `minSize`, `maxSize`                                                                                                 |
| `text`                     | `Text`                 | `minSize`, `maxSize`                                                                                                 |
| `char`                     | `Char`                 | `codec`, `minCodepoint`, `maxCodepoint`, `categories`, `excludeCategories`, `includeCharacters`, `excludeCharacters` |
| `uuid`                     | `UUID`                 | `version`                                                                                                            |
| `uri`, `uriText`           | parsed / raw URIs      | —                                                                                                                    |
| `domain`                   | domain names           | `maxLength`                                                                                                          |
| `regex`                    | strings matching regex | `fullMatch`, `alphabet`                                                                                              |
| `list`                     | linked lists           | `minSize`, `maxSize`, `unique`                                                                                       |
| `set`, `hashSet`, `intSet` | set variants           | `minSize`, `maxSize`                                                                                                 |
| `map`, `hashMap`, `intMap` | map variants           | `minSize`, `maxSize`                                                                                                 |

| Combinator                                     | Purpose                                                                           |
| ---------------------------------------------- | ----------------------------------------------------------------------------------|
| `oneOf :: [Gen a] -> Gen a`                    | Choose from a list of generators; the list must be non-empty                      |
| `element :: [a] -> Gen a`                      | Choose from a list of values; the list must be non-empty                          |
| `frequency :: [(Int, Gen a)] -> Gen a`         | Weighted choice; all weights must be positive                                     |
| `maybe :: Gen a -> Gen (Maybe a)`              | `Nothing` or `Just` a generated value                                             |
| `either :: Gen a -> Gen b -> Gen (Either a b)` | `Left` from the first generator or `Right` from the second                        |
| `assume :: Bool -> Gen ()`                     | Conditionally discard the current test case                                       |
| `discard :: Gen a`                             | Unconditionally discard the current test case                                     |
| `defer :: Gen a -> Gen a`                      | Force interactive generation; required on recursive edges                         |
| `filtered :: (a -> Bool) -> Gen a -> Gen a`    | Retry until the predicate holds _or_ the retry budget exhausts                    |
| `mapMaybe :: (a -> Maybe b) -> Gen a -> Gen b` | Retry until we draw a value that produces `Just b` _or_ the retry budget exhausts |
| `just :: Gen (Maybe a) -> Gen a`               | Retry until we draw `Just a` _or_ the retry budget exhausts                       |
| `enumerate :: Gen a -> Maybe [a]`              | Attempt to enumerate a finite generator's possible values                         |

## Development

Clone the repository and enter the development shell with `nix develop`.

Common development actions can performed with the `just` command runner, for example:

```shell
$ just check             # CI checks: check-format + build + test
$ just build             # compile the library & test suite
$ just test              # run the unit test suite
$ just test <name>       # run a specific test suite
$ just check-conformance # build the conformance binaries and run the pytest harness
$ just format            # run all formatters
$ just check-format      # verify formatting without modifying files
$ just docs              # build Haddocks
$ just repl              # start a GHCi session with this library in-scope
```

## Frequently Asked Questions

### What's missing?

`zizek` passes `hegel-core`'s conformance tests, but some work remains outstanding:

* an API to seed the `Explicit` phase with hand-written examples
* stateful/state-machine testing
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
