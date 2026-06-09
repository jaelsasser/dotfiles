#!/usr/bin/env bash
# emacs.sh — scoped startup-time bench for the live emacs config (~/.config/emacs).
# Report-only: prints a table, never fails.
#   total  — foreground config load (before-init-time → probe), median of N batch runs.
#   scoped — per-package init+config time from use-package-statistics.
# Two footguns drive the design:
#   * `--batch` alone does NOT load the user init, so both runs explicitly -l early-init
#     -l init (the emacs.bats load path). `-q` lets us set use-package-compute-statistics
#     *before* init's use-package forms run — the only injection point that early.
#   * native-comp is synchronous under --batch (async in a real launch), so an un-cached
#     .eln recompiles on the hot path and dwarfs everything. native-comp-jit-compilation
#     nil keeps the load to byte-code, which is what a warm real startup actually pays;
#     a warm-up run absorbs the rest. Numbers are foreground load, GUI frame excluded.
set -u
command -v emacs >/dev/null 2>&1 || { echo "emacs startup — not installed, skipping"; exit 0; }

E="${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
{ [ -f "$E/early-init.el" ] && [ -f "$E/init.el" ]; } || { echo "emacs startup — no ~/.config/emacs init, skipping"; exit 0; }
PROBE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/emacs.el"
RUNS=${BENCH_RUNS:-10}; [ "$RUNS" -ge 1 ] 2>/dev/null || RUNS=10
export LC_NUMERIC=C   # the probe writes '.'-decimals; keep printf parsing them under any locale
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# $1 -> BENCH_OUT; pass --eval forms / extra -l after.
run() {
  local out=$1; shift
  BENCH_OUT="$out" emacs --batch -q \
    --eval '(setq native-comp-jit-compilation nil)' \
    -l "$E/early-init.el" "$@" -l "$E/init.el" -l "$PROBE" >/dev/null 2>&1
}

run "$TMP/warm"   # warm-up: trampolines + fs cache (discarded)

for ((i = 0; i < RUNS; i++)); do
  run "$TMP/t.$i"
  awk '/^TOTAL/ { print $2 }' "$TMP/t.$i" 2>/dev/null   # a crashed run writes no file; the n==0 guard reports it
done | sort -g > "$TMP/totals"

run "$TMP/scoped" --eval '(setq use-package-compute-statistics t)'

n=$(wc -l < "$TMP/totals")
[ "$n" -gt 0 ] || { echo "emacs startup — config did not load under --batch, skipping"; exit 0; }
med=$(sed -n "$(((n + 1) / 2))p" "$TMP/totals")
min=$(head -1 "$TMP/totals"); max=$(tail -1 "$TMP/totals")

printf 'emacs startup — live config (~/.config/emacs), N=%d, batch load (no GUI frame)\n' "$RUNS"
printf '  total: median %.1f ms  (min %.1f, max %.1f)\n' "$med" "$min" "$max"
printf '  scoped (use-package init+config):\n'
awk '/^PKG/ { print $2 "\t" $3 }' "$TMP/scoped" | head -14 | while IFS=$'\t' read -r ms name; do
  printf '    %-22s %8.1f ms\n' "$name" "$ms"
done
printf '  note: eagerly-required packages (no :defer/:commands) load on the hot path;\n'
printf '        per-package times sum overlapping use-package phase timers.\n'
