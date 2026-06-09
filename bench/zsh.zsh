#!/usr/bin/env zsh
# zsh.zsh — scoped startup-time bench for the live zsh config (~/.config/zsh).
# Report-only: prints a table, never fails. Three lenses:
#   total  — wall-clock of a real interactive startup, `zsh -i -c exit` × N.
#   split  — .zshenv (env, non-interactive) vs the .zshrc interactive remainder.
#   scoped — per-function self time via zprof.
# Footgun: /etc/zshenv force-exports ZDOTDIR, so a temp-ZDOTDIR zprof inject loses
# the fight (and -d/--no-globalrcs don't skip it here). `zsh -f` (no rc files at all)
# + a manual source of the real .zshenv/.zshrc sidesteps it — that's the scoped run.
set -u
zmodload zsh/datetime
export LC_NUMERIC=C   # EPOCHREALTIME math, sort -g, and printf must share the '.' radix

RUNS=${BENCH_RUNS:-10}
[[ $RUNS == <1-> ]] || RUNS=10
ZCFG=${XDG_CONFIG_HOME:-$HOME/.config}/zsh

# median min max, from numbers on stdin
_stats() {
  local -a v=( "${(@f)$(sort -g)}" )
  print -r -- "${v[($#v + 1) / 2]} ${v[1]} ${v[-1]}"
}

# wall-clock of `zsh $@ -c exit`, in ms
_time_ms() {
  local s=$EPOCHREALTIME
  command zsh "$@" -c exit
  print -r -- $(( (EPOCHREALTIME - s) * 1000 ))
}

local -a full env
for ((i = 0; i < RUNS; i++)); do
  full+=( $(_time_ms -i) )   # interactive: .zshenv + .zshrc
  env+=(  $(_time_ms) )      # env only:    .zshenv
done

read full_med full_min full_max <<< "$(print -l $full | _stats)"
read env_med  _         _       <<< "$(print -l $env  | _stats)"

print -r -- "zsh startup — live config ($ZCFG), N=$RUNS"
printf '  total: median %.1f ms  (min %.1f, max %.1f)\n' $full_med $full_min $full_max
printf '  split: .zshenv %.1f ms  |  .zshrc (interactive) %.1f ms\n' \
       $env_med $(( full_med - env_med ))
print -r -- '  scoped (zprof, self time):'

# zprof emits a summary table (one row/function) then per-function callgraph blocks
# whose headers also start with `N)`; read only the summary (header → first blank).
zsh -f -i -c "
  zmodload zsh/zprof
  source ${(qq)ZCFG}/.zshenv
  source ${(qq)ZCFG}/.zshrc
  zprof
" 2>/dev/null \
  | awk '
      /^num  calls/      { t = 1; next }
      t && NF == 0       { exit }
      t && /^ *[0-9]+\)/ {
        sub(/%/, "", $8); name = $9
        for (i = 10; i <= NF; i++) name = name " " $i
        print $6 "\t" $8 "\t" name
      }' \
  | sort -gr | head -12 \
  | while IFS=$'\t' read -r self pct name; do
      printf '    %-34s %7.2f ms  %5.1f%%\n' "$name" "$self" "$pct"
    done
