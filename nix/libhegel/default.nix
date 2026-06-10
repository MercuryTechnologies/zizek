{ lib
, stdenv
, rustPlatform
, fetchFromGitHub
, fixDarwinDylibNames
}:

rustPlatform.buildRustPackage {
  pname = "libhegel";
  version = "0.17.0";

  src = fetchFromGitHub {
    owner = "hegeldev";
    repo = "hegel-rust";
    rev = "v0.17.0";
    hash = "sha256-DG/GNzBr0M6+Tji6K9BxcXW/a85jTk5GZcbeXVHIEjc=";
  };

  cargoHash = "sha256-o67Wk3AIm/c0CDwwA0dV5h0gat70QewK/MlIjJNRW6I=";

  # Build only the C-binding crate.
  cargoBuildFlags = [ "--package" "hegeltest-c" ];

  nativeBuildInputs = lib.optionals stdenv.hostPlatform.isDarwin [ fixDarwinDylibNames ];

  # Tests require a running hegel instance; skip in the Nix sandbox.
  doCheck = false;

  outputs = [ "out" "dev" ];

  installPhase = ''
    runHook preInstall

    releaseDir="target/${stdenv.hostPlatform.rust.rustcTarget}/release"

    # Install shared library and static archive to $out.
    mkdir -p $out/lib
    install -m644 "$releaseDir"/libhegel.* $out/lib/

    # Install cbindgen-generated C header and pkg-config file to $dev.
    mkdir -p $dev/include $dev/lib/pkgconfig
    install -m644 hegel-c/include/hegel.h $dev/include/
    substituteAll ${./hegel.pc.in} $dev/lib/pkgconfig/hegel.pc

    runHook postInstall
  '';

  meta = {
    description = "Native Hypothesis engine C library (hegeltest-c)";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
