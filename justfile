hs_dirs  := "library tests"
nix_dirs := "."

# Format cabal, Haskell, and Nix sources in one shot.
format:
  @cabal-gild -i zizek.cabal -o zizek.cabal
  @find {{hs_dirs}} -name '*.hs' | xargs ormolu --mode inplace
  @find {{nix_dirs}} -name '*.nix' -not -path './dist-newstyle/*' | xargs nixpkgs-fmt

# Verify formatting without modifying files (for CI / pre-commit).
check-format:
  @cabal-gild -i zizek.cabal -o /dev/stdout | diff -u zizek.cabal -
  @find {{hs_dirs}} -name '*.hs' | xargs ormolu --mode check

# Run the same checks CI runs: check-format, build, test.
check: check-format build test

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

# Run hlint over the library + tests (add hlint to flake.nix:devShells first).
lint:
  @echo "lint: not yet implemented — add hlint to flake.nix devShell and wire up here"
  @exit 1

# Build all conformance test binaries and install symlinks (shared helper).
_conformance-build:
  @cabal build zizek:test-booleans zizek:test-binary zizek:test-floats zizek:test-integers zizek:test-integers-narrow zizek:test-frequency zizek:test-list zizek:test-set zizek:test-map zizek:test-origin-deduplication zizek:test-sampled-from zizek:test-one-of zizek:test-text zizek:test-char zizek:test-regex
  @mkdir -p tests/conformance/pytest/bin
  @ln -sf $(cabal list-bin zizek:test-booleans) tests/conformance/pytest/bin/test-booleans
  @ln -sf $(cabal list-bin zizek:test-binary) tests/conformance/pytest/bin/test-binary
  @ln -sf $(cabal list-bin zizek:test-floats) tests/conformance/pytest/bin/test-floats
  @ln -sf $(cabal list-bin zizek:test-integers) tests/conformance/pytest/bin/test-integers
  @ln -sf $(cabal list-bin zizek:test-integers-narrow) tests/conformance/pytest/bin/test-integers-narrow
  @ln -sf $(cabal list-bin zizek:test-frequency) tests/conformance/pytest/bin/test-frequency
  @ln -sf $(cabal list-bin zizek:test-list) tests/conformance/pytest/bin/test-list
  @ln -sf $(cabal list-bin zizek:test-set) tests/conformance/pytest/bin/test-set
  @ln -sf $(cabal list-bin zizek:test-map) tests/conformance/pytest/bin/test-map
  @ln -sf $(cabal list-bin zizek:test-origin-deduplication) tests/conformance/pytest/bin/test-origin-deduplication
  @ln -sf $(cabal list-bin zizek:test-sampled-from) tests/conformance/pytest/bin/test-sampled-from
  @ln -sf $(cabal list-bin zizek:test-one-of) tests/conformance/pytest/bin/test-one-of
  @ln -sf $(cabal list-bin zizek:test-text) tests/conformance/pytest/bin/test-text
  @ln -sf $(cabal list-bin zizek:test-char) tests/conformance/pytest/bin/test-char
  @ln -sf $(cabal list-bin zizek:test-regex) tests/conformance/pytest/bin/test-regex

# Run the Python conformance harness using the native backend.
check-conformance: _conformance-build
  @pytest tests/conformance/pytest/ -n auto

# --- Stubs: implement when the underlying tooling lands. ---

# Build with coverage and produce a report (add hpc-codecov to flake.nix first).
check-coverage:
  @echo "check-coverage: not yet implemented — add hpc-codecov to flake.nix devShell and wire up here"
  @exit 1
