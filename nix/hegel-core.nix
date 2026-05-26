{ lib, python3Packages }:

let
  # nixpkgs pins hypothesis 6.136.9; observability_enabled was added in 6.137.0.
  # Build from the wheel to avoid the upstream monorepo sourceRoot issue.
  hypothesis_6_152_9 = python3Packages.buildPythonPackage rec {
    pname = "hypothesis";
    version = "6.152.9";
    format = "wheel";
    src = python3Packages.fetchPypi {
      inherit pname version;
      format = "wheel";
      dist = "py3";
      python = "py3";
      abi = "none";
      platform = "any";
      hash = "sha256-nE/cyx6sCxLsdAwSKQ0OagvqNSaj8L+BK3ZDu1Y8LYs=";
    };
    doCheck = false;
  };
in
python3Packages.buildPythonPackage rec {
  pname = "hegel-core";
  version = "0.9.1";
  format = "wheel";

  src = python3Packages.fetchPypi {
    pname = "hegel_core";
    inherit version;
    format = "wheel";
    dist = "py3";
    python = "py3";
    abi = "none";
    platform = "any";
    hash = "sha256-YiKGbyeS6LR1dD+BR0aD2Lq3eBYsO+3gDMEkWLp+mBw=";
  };

  dependencies = with python3Packages; [
    cbor2
    click
    hypothesis_6_152_9
    sortedcontainers
  ];

  # Skip the test suite — it requires the Rust client to be present.
  doCheck = false;

  meta = with lib; {
    description = "hegel-core server: Hypothesis-backed property test engine";
    license = licenses.mit;
    mainProgram = "hegel";
  };
}
