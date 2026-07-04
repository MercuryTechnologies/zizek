{
  description = "Property-based testing for Haskell, built on Hypothesis.";

  outputs =
    { self, nixpkgs, ... }:
    let
      forAllSystems =
        withPkgs:
        nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
          system:
          withPkgs {
            inherit system;
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ self.overlays.default ];
            };
          }
        );
    in
    {
      packages = forAllSystems ({ pkgs, ... }: {
        inherit (pkgs) libhegel;
      });

      devShells = forAllSystems (
        { pkgs, ... }:
        {
          default = pkgs.mkShell {
            buildInputs =
              with pkgs;
              [
                # task runner
                just
                # nix tools
                nixpkgs-fmt
                # haskell dev tools
                cabal-install
                haskell.compiler.ghc912
                haskellPackages.cabal-gild
                haskellPackages.ormolu
                # misc dev dependencies
                repomix
                tokei
                zlib.dev
                # profiling & benchmarking tools (just profile-*)
                hyperfine
                haskellPackages.eventlog2html
                haskellPackages.ghc-prof-flamegraph
                haskellPackages.profiterole
                haskellPackages.profiteur
                # native libhegel C library + pkg-config for discovery
                libhegel
                pkg-config
              ]
              ++ lib.optionals stdenv.hostPlatform.isDarwin [
                apple-sdk_15
              ];
          };
        }
      );

      overlays = {
        default = final: _prev: {
          libhegel = final.callPackage ./nix/libhegel { };
        };
      };
    };

  inputs = {
    nixpkgs.url = "flake:nixpkgs";
  };
}
