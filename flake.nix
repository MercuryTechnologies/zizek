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
        libhegel = pkgs.callPackage ./nix/libhegel { };
      });

      devShells = forAllSystems (
        { pkgs, self', ... }:
        let
          python = pkgs.python3.withPackages (ps: [
            self'.packages.hegel-core
            ps.pytest
            ps."pytest-subtests"
            ps."pytest-xdist"
          ]);
        in
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
                # python interpreter with hegel-core + conformance test deps
                python
                # native libhegel C library + pkg-config for discovery
                self'.packages.libhegel
                pkg-config
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
