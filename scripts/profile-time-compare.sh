#!/usr/bin/env bash
# Per-scenario wall-clock A/B of two profile-hegel builds, one hyperfine
# invocation per scenario so the relative ratio prints alongside the means.
# The scenario list comes from `<bin-a> --list` (plus the shrink --no-shrink
# variant), so new scenarios are compared automatically.
#
# `just profile-time-compare` drives this as -O1 vs -O0 (the consumer build
# vs the un-optimized dev loop), but the labels are free-form — it works for
# any two builds, e.g. binaries from two commits.
#
# Usage: scripts/profile-time-compare.sh <label-a> <bin-a> <label-b> <bin-b>
# Env:   OUT  output directory for the exported tables (default: profiles/compare)
set -euo pipefail

if [[ $# -ne 4 ]]; then
  echo "usage: $0 <label-a> <bin-a> <label-b> <bin-b>" >&2
  exit 2
fi

label_a=$1
bin_a=$2
label_b=$3
bin_b=$4
out=${OUT:-profiles/compare}

if ! command -v hyperfine >/dev/null 2>&1; then
  echo "error: hyperfine not found (enter the nix dev shell)" >&2
  exit 1
fi

mkdir -p "$out"

# compare <table-name> <scenario args...> — the inner quotes keep a spaced
# binary path intact inside the command string hyperfine hands to its shell.
compare() {
  local name=$1
  shift
  hyperfine --warmup 2 --runs 10 \
    --export-markdown "$out/$name.md" \
    --command-name "$label_a $*" "\"$bin_a\" $*" \
    --command-name "$label_b $*" "\"$bin_b\" $*"
}

while read -r scenario _; do
  compare "$scenario" "$scenario"
done < <("$bin_a" --list)
compare "shrink-no-shrink" shrink --no-shrink

echo "wrote per-scenario tables to $out/"
