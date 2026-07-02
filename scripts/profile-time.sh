#!/usr/bin/env bash
# Wall-clock comparison of the profiling scenarios on the -O2 non-profiled
# (release) build, via hyperfine. Includes the shrink / shrink --no-shrink
# pair for Shrink-phase attribution. All scenarios run with the harness's
# fixed default seed, so every iteration does identical work.
#
# Usage: scripts/profile-time.sh <release-bin>
# Env:   OUT  output directory for the exported results (default: profiles)
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <release-bin>" >&2
  exit 2
fi

bin=$1
out=${OUT:-profiles}

if ! command -v hyperfine >/dev/null 2>&1; then
  echo "error: hyperfine not found (enter the nix dev shell)" >&2
  exit 1
fi

mkdir -p "$out"

hyperfine --warmup 2 --runs 10 \
  --export-markdown "$out/wallclock.md" \
  --export-json "$out/wallclock.json" \
  --command-name 'overhead (10k cases, 1 draw each)'   "$bin overhead" \
  --command-name 'scalars (1k cases, 100 draws each)'  "$bin scalars" \
  --command-name 'collections (500 cases)'             "$bin collections" \
  --command-name 'stateful-simple (2k cases)'          "$bin stateful-simple" \
  --command-name 'warehouse (1k cases)'                "$bin warehouse" \
  --command-name 'shrink (full phases)'                "$bin shrink" \
  --command-name 'shrink (generate only)'              "$bin shrink --no-shrink"

echo "wrote $out/wallclock.md and $out/wallclock.json"
