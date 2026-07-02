#!/usr/bin/env bash
# Capture cost-centre (.prof), heap (.hp), eventlog, and GC-summary (.rts)
# profiles for one scenario of the profiling-way profile-hegel binary.
# Renders .prof / .eventlog to HTML when profiteur / eventlog2html are on
# PATH (best-effort; the raw files are readable without them).
#
# Usage: scripts/profile-space.sh <profile-bin> <scenario> [args...]
# Env:   OUT  output directory (default: profiles)
#
# Interpretation guide: notes/04-profiling-harness.md.
set -euo pipefail

if [[ $# -lt 2 ]]; then
  echo "usage: $0 <profile-bin> <scenario> [args...]" >&2
  exit 2
fi

bin=$1
scenario=$2
shift 2

out=${OUT:-profiles}
stem="$out/$scenario"
mkdir -p "$out"

# -p   cost-centre profile (-> stem.prof, via -po)
# -hc  heap profile by cost centre (-> stem.hp, via -po)
# -l   eventlog (-> stem.eventlog, via -ol)
# -s   GC/RTS summary (-> stem.rts)
"$bin" "$scenario" "$@" \
  +RTS -N -p -hc -l -s"${stem}.rts" -po"${stem}" -ol"${stem}.eventlog" -RTS

if command -v profiteur >/dev/null 2>&1; then
  profiteur "${stem}.prof" >/dev/null
fi
if command -v eventlog2html >/dev/null 2>&1; then
  eventlog2html "${stem}.eventlog" >/dev/null
fi

echo "wrote ${stem}.prof(.html), ${stem}.hp, ${stem}.eventlog(.html), ${stem}.rts"
