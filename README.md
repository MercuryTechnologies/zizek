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

## Installation

> [!IMPORTANT]
> `zizek` depends on the [`hegel-core`] server; it is made available in the Nix dev shell, but any downstream consumers will need to ensure that `hegel` is available on their `$PATH`.

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

[`hegel-core`]: https://pypi.org/project/hegel-core/

## Quickstart

> [!TIP]
> `zizek` is in active development and lacks compatibility layers for common testing libraries that would allow developers to start quickly; for the time being you are encouraged to read the tests in this repository to understand how to use the library directly if you wish to do so.

### Usage

`zizek` tries to wrap the underlying `hegel-core` machinery in a higher-level API for constructing and exercising complex `Generator`s.

#### Simple Generators

For example, consider the following property that generates machine integers in the range `[0,1000]` and then validates trivial property that all of these `n + 1 > n`:

```haskell
import Control.Monad (unless)
import Data.Function ((&))
import Hegel (defaultSettings, runProperty_)
import Hegel.Gen qualified as Gen

prop_successor :: IO ()
prop_successor = do
  let ints = Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build
  runProperty_ defaultSettings ints $ \n ->
    unless (n + 1 > n) (error "successor should be greater")
```

#### Independent Generators

When the draws in a `do` block don't reference each other, and `ApplicativeDo` has been enabled, `zizek` will batch them into a single request to `hegel-core` for the whole tuple and the server can shrink each component independently when a counterexample is found:

```haskell
{-# LANGUAGE ApplicativeDo #-}

import Control.Monad (unless)
import Data.Function ((&))
import Hegel (defaultSettings, runProperty_)
import Hegel.Gen (Generator)
import Hegel.Gen qualified as Gen

boolAndInt :: Generator (Bool, Int)
boolAndInt = do
  b <- Gen.bool                           & Gen.build
  n <- Gen.int & Gen.min 0 & Gen.max 100  & Gen.build
  pure (b, n)

prop_pair :: IO ()
prop_pair = runProperty_ defaultSettings boolAndInt $ \(_, n) ->
  unless (n >= 0 && n <= 100) (error "second component out of range")
```

> [!NOTE]
> The example will compile and run without `ApplicativeDo`, but `zizek` will now issue two calls to `hegel-core` and shrinking will be dependent.

#### Dependent Generators

When a later draw needs to look at an earlier one, each must be sequenced as part of its own request. This lets us establish dependent relationships between generated values.

For example, `Gen.assume` lets you state preconditions inline; if the condition fails, the test case is discarded rather than counted as a failure:

```haskell
import Control.Monad (unless)
import Data.Function ((&))
import Hegel (defaultSettings, runProperty_)
import Hegel.Gen (Generator)
import Hegel.Gen qualified as Gen

interval :: Generator (Int, Int)
interval = do
  lo <- Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
  hi <- Gen.int & Gen.min 0 & Gen.max 100 & Gen.build
  Gen.assume (lo < hi)
  pure (lo, hi)

prop_intervalOrdered :: IO ()
prop_intervalOrdered = runProperty_ defaultSettings interval $ \(lo, hi) ->
  unless (lo < hi) (error "interval invariant broken")
```

> [!TIP]
> Unlike `Gen.filtered`, `assume` does not retry; if the predicate rejects too often, `runProperty` reports a `Rejected` outcome.

### Development

Clone the repository and enter the development shell with `nix develop`.

Common development actions can performed with the `just` command runner, for example:

```shell
$ just build             # compile the library & test suite
$ just test              # run the full test suite
$ just test suite=<name> # run a subset of the full test suite
$ just format            # run all formatters
$ just format-check      # lint formatting
$ just docs              # generate Haddocks
```
