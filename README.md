# Hegel for Haskell

> "Beyond the fiction of reality, there is the reality of the fiction."
>
> Slavoj Žižek[^1]

`zizek` is a property-based testing library for Haskell; it is based on [Hypothesis] and uses the [Hegel protocol].

[Hypothesis]: https://github.com/hypothesisworks/hypothesis
[Hegel protocol]: https://hegel.dev/reference/protocol

[^1]: [It's a philosophy joke.](https://antithesis.com/blog/2026/hegel/)

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
