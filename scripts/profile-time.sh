#!/usr/bin/env bash
# Wall-clock comparison of all profiling scenarios on the default (-O1,
# non-profiled) build — the configuration consumers' test suites actually
# run — via hyperfine. The scenario list comes from `profile-hegel --list`
# (single source of truth), plus the shrink / shrink --no-shrink pair for
# Shrink-phase attribution. All scenarios run with the harness's fixed
# default seed, so every iteration does identical work.
#
# Usage: scripts/profile-time.sh <profile-hegel-bin>
# Env:   OUT  output directory for the exported results (default: profiles)
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <profile-hegel-bin>" >&2
  exit 2
fi

bin=$1
out=${OUT:-profiles}

if ! command -v hyperfine >/dev/null 2>&1; then
  echo "error: hyperfine not found (enter the nix dev shell)" >&2
  exit 1
fi

mkdir -p "$out"

# One benchmark per scenario, labelled by bare scenario name (case counts and
# blurbs live in the harness — `--list` shows them; restating them here would
# go stale). The inner quotes keep a spaced $bin path intact inside the
# command string hyperfine hands to its shell.
benchmarks=()
while read -r scenario _; do
  benchmarks+=(--command-name "$scenario" "\"$bin\" $scenario")
done < <("$bin" --list)
benchmarks+=(--command-name 'shrink --no-shrink' "\"$bin\" shrink --no-shrink")

hyperfine --warmup 2 --runs 10 \
  --export-markdown "$out/wallclock.md" \
  --export-json "$out/wallclock.json" \
  "${benchmarks[@]}"

echo "wrote $out/wallclock.md and $out/wallclock.json"
