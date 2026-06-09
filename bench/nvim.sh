#!/usr/bin/env bash
# nvim.sh — scoped startup-time bench for the live nvim config (~/.config/nvim).
# Report-only: prints a table, never fails. Drives `nvim --startuptime` × N and reads
# the log of the median run two ways:
#   phases     — nvim's own macro timeline (2-number lines: clock, elapsed).
#   components — every `sourcing <path>` / `require('mod')` (3-number lines), bucketed
#                by a classifier and summed on *self* time (the 3rd column) so nested
#                requires aren't double-counted against their parent.
set -u
command -v nvim >/dev/null 2>&1 || { echo "nvim startup — not installed, skipping"; exit 0; }

RUNS=${BENCH_RUNS:-10}; [ "$RUNS" -ge 1 ] 2>/dev/null || RUNS=10
export LC_NUMERIC=C   # the startuptime log floats and our printf must share the '.' radix
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

nvim --startuptime /dev/null +qa >/dev/null 2>&1 || true   # warm ShaDa/fs caches, discard

for ((i = 0; i < RUNS; i++)); do
  log="$TMP/run.$i"
  nvim --startuptime "$log" +qa >/dev/null 2>&1
  awk -v l="$log" '/--- NVIM STARTED ---/ { print $1, l }' "$log"
done | sort -n > "$TMP/totals"

n=$(wc -l < "$TMP/totals")
[ "$n" -gt 0 ] || { echo "nvim startup — no run reached '--- NVIM STARTED ---', skipping"; exit 0; }
read -r med medlog < <(sed -n "$(((n + 1) / 2))p" "$TMP/totals")
min=$(head -1 "$TMP/totals" | cut -d' ' -f1)
max=$(tail -1 "$TMP/totals" | cut -d' ' -f1)

printf 'nvim startup — live config (~/.config/nvim), N=%d\n' "$RUNS"
printf '  total: median %.1f ms  (min %.1f, max %.1f)\n' "$med" "$min" "$max"

printf '  phases (self):\n'
awk '
  /--- NVIM (STARTING|STARTED) ---/   { next }
  /^[ ]*[0-9.]+[ ]+[0-9.]+: / { el = $2; sub(/:/, "", el)
                                name = $3; for (i = 4; i <= NF; i++) name = name " " $i
                                print el "\t" name }
' "$medlog" | sort -gr | head -8 | while IFS=$'\t' read -r el name; do
  printf '    %-26s %7.2f ms\n' "$name" "$el"
done

printf '  components (self, bucketed):\n'
awk '
  /^[ ]*[0-9.]+[ ]+[0-9.]+[ ]+[0-9.]+: / {
    self = $3; sub(/:/, "", self)
    name = $4; for (i = 5; i <= NF; i++) name = name " " $i
    if      (name ~ /nvim-treesitter/)        b = "nvim-treesitter"
    else if (name ~ /mini\./)                 b = "mini.nvim"
    else if (name ~ /flash/)                  b = "flash.nvim"
    else if (name ~ /vim-rsi/)                b = "vim-rsi"
    else if (name ~ /lspconfig/)              b = "nvim-lspconfig"
    else if (name ~ /vim\.lsp/)               b = "vim.lsp (builtin)"
    else if (name ~ /vim\.treesitter/)        b = "vim.treesitter (builtin)"
    else if (name ~ /vim\.diagnostic/)        b = "vim.diagnostic (builtin)"
    else if (name ~ /\/vim\/vimrc/)           b = "vim spine"
    else if (name ~ /\/nvim\/init\.lua/)      b = "init.lua"
    else if (name ~ /share\/nvim\/runtime/)   b = "nvim runtime"
    else if (name ~ /require\(.vim[._]/)      b = "nvim core (lua)"
    else                                      b = "other"
    sums[b] += self
  }
  END { for (b in sums) printf "%.3f\t%s\n", sums[b], b }
' "$medlog" | sort -gr | head -12 | while IFS=$'\t' read -r ms name; do
  printf '    %-26s %7.2f ms\n' "$name" "$ms"
done
