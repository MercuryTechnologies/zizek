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
            self'.packages = self.packages.${system};
          }
        );
    in
    {
      packages = forAllSystems ({ pkgs, ... }: {
        hegel-core = (pkgs.callPackage ./nix/hegel-core.nix { });
      });

      devShells = forAllSystems (
        { pkgs, self', ... }:
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
                zlib.dev
                # hegel-core server
                self'.packages.hegel-core
              ]
              ++ lib.optionals stdenv.hostPlatform.isDarwin [
                apple-sdk_15
              ];
          };
        }
      );

      overlays = {
        default = _: _: { };
      };
    };

  inputs = {
    nixpkgs.url = "flake:nixpkgs";
  };
}
