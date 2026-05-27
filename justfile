hs_dirs  := "library tests"
nix_dirs := "."

# Format cabal, Haskell, and Nix sources in one shot.
format:
  @cabal-gild -i zizek.cabal -o zizek.cabal
  @find {{hs_dirs}} -name '*.hs' | xargs ormolu --mode inplace
  @find {{nix_dirs}} -name '*.nix' -not -path './dist-newstyle/*' | xargs nixpkgs-fmt

# Verify formatting without modifying files (for CI / pre-commit).
format-check:
  @cabal-gild -i zizek.cabal -o /dev/stdout | diff -u zizek.cabal -
  @find {{hs_dirs}} -name '*.hs' | xargs ormolu --mode check

# Run the same checks CI runs: format-check, build, test.
check: format-check build test

build target="all":
  cabal build {{target}}

test suite="unit":
  cabal test zizek:{{suite}}

# Count lines of code in library, test, and nix sources.
loc:
  @tokei {{hs_dirs}} nix

# Build API docs with links to Hackage for dependencies.
# Note: source-repository-package deps (wireform-*) are not on Hackage; their links will 404.
docs:
  cabal haddock \
    --enable-documentation \
    --haddock-hyperlink-source \
    --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs'

# Drop build artifacts.
clean:
  cabal clean

# Open a REPL on the library.
repl:
  cabal repl zizek

# --- Stubs: implement when the underlying tooling lands. ---

# Run hlint over the library + tests (add hlint to flake.nix:devShells first).
lint:
  @echo "lint: not yet implemented — add hlint to flake.nix devShell and wire up here"
  @exit 1

# Run the Python conformance harness against the Haskell test binaries.
check-conformance:
  @cabal build zizek:test-booleans zizek:test-binary zizek:test-floats zizek:test-integers zizek:test-origin-deduplication zizek:test-sampled-from zizek:test-one-of
  @mkdir -p tests/conformance/bin
  @ln -sf $(cabal list-bin zizek:test-booleans) tests/conformance/bin/test-booleans
  @ln -sf $(cabal list-bin zizek:test-binary) tests/conformance/bin/test-binary
  @ln -sf $(cabal list-bin zizek:test-floats) tests/conformance/bin/test-floats
  @ln -sf $(cabal list-bin zizek:test-integers) tests/conformance/bin/test-integers
  @ln -sf $(cabal list-bin zizek:test-origin-deduplication) tests/conformance/bin/test-origin-deduplication
  @ln -sf $(cabal list-bin zizek:test-sampled-from) tests/conformance/bin/test-sampled-from
  @ln -sf $(cabal list-bin zizek:test-one-of) tests/conformance/bin/test-one-of
  @pytest tests/conformance/

# Build with coverage and produce a report (add hpc-codecov to flake.nix first).
check-coverage:
  @echo "check-coverage: not yet implemented — add hpc-codecov to flake.nix devShell and wire up here"
  @exit 1
