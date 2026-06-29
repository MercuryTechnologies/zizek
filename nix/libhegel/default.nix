{ lib
, stdenv
, rustPlatform
, fetchFromGitHub
, fixDarwinDylibNames
, patchelf
, testers
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "libhegel";
  version = "0.23.2";

  src = fetchFromGitHub {
    owner = "hegeldev";
    repo = "hegel-rust";
    rev = "v0.23.2";
    hash = "sha256-Dwh7vMQ8+3z3SCO9k1JBjYsg6Jq25tZtfRACerQCmjc=";
  };

  cargoHash = "sha256-slZ5/dw5vPyVUktaqGfiw218Eoc6pLAmtO6leRt3LDo=";

  # Build only the C-binding crate.
  cargoBuildFlags = [ "--package" "hegeltest-c" ];

  nativeBuildInputs =
    lib.optionals stdenv.hostPlatform.isDarwin [ fixDarwinDylibNames ]
    ++ lib.optionals stdenv.hostPlatform.isElf [ patchelf ];

  # Tests require a running hegel instance; skip in the Nix sandbox.
  doCheck = false;

  outputs = [ "out" "dev" ];

  installPhase = ''
    runHook preInstall

    releaseDir="target/${stdenv.hostPlatform.rust.rustcTarget}/release"

    # Install shared library and static archive to $out.
    #
    # NOTE: Upstream renames the release artifact from libhegel_c to libhegel;
    # we do the same here so anyone linking against the pre-compiled libraries
    # doesn't have to jump through any extra hoops to do so.
    mkdir -p $out/lib
    install -m644 "$releaseDir/libhegel_c.a" "$out/lib/libhegel.a"
    install -m644 \
      "$releaseDir/libhegel_c${stdenv.hostPlatform.extensions.sharedLibrary}" \
      "$out/lib/libhegel${stdenv.hostPlatform.extensions.sharedLibrary}"

    # rustc bakes the crate's lib name into the ELF soname (libhegel_c.so), so
    # the rename above leaves the soname pointing at a file that no longer
    # exists; downstream links would record that stale soname as DT_NEEDED and
    # fail to load at runtime.
    ${lib.optionalString (stdenv.hostPlatform.isElf && !stdenv.hostPlatform.isStatic) ''
      patchelf --set-soname libhegel.so "$out/lib/libhegel.so"
    ''}

    # Install cbindgen-generated C header and pkg-config file to $dev.
    mkdir -p $dev/include $dev/lib/pkgconfig
    install -m644 hegel-c/include/hegel.h $dev/include/
    substituteAll ${./hegel.pc.in} $dev/lib/pkgconfig/hegel.pc

    runHook postInstall
  '';

  passthru.tests.pkgConfig = testers.testMetaPkgConfig finalAttrs.finalPackage;

  meta = {
    description = "Native Hypothesis engine C library (hegeltest-c)";
    homepage = "https://github.com/hegeldev/hegel-rust";
    changelog = "https://github.com/hegeldev/hegel-rust/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.mit;
    platforms = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
    pkgConfigModules = [ "hegel" ];
  };
})
