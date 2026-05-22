format:
  @cabal-gild -i zizek.cabal -o zizek.cabal
  @ormolu --mode inplace $(find . -name '*.hs')

format-nix:
  nixpkgs-fmt $(find -name '*.nix')

build target: (fmt target)
  cabal build {{target}}

test target suite="unit":
  cabal test {{target}}:{{suite}}

