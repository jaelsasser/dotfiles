# AGENTS.md — startup benches

`bench/` holds three scoped startup-time benches over the **live deployed config** (`~/.config/{zsh,nvim,emacs}`) — warm, read-only, **report-only**: they print a table and never fail, because a startup time isn't pass/fail (a bench, not a test). Dev-only, **never deployed**; it measures the live deployed config, *not* a throwaway. These are **not** tests — nothing here asserts; report-only and machine-dependent.

## Commands

```bash
task bench           # zsh + nvim + emacs scoped startup tables
task bench:zsh       # one tool; resample with e.g. BENCH_RUNS=20 task bench:nvim
```

`BENCH_RUNS` (default 10) sets the sample count.

## The three lenses, and the footgun that shaped each

- **`zsh.zsh`** — `EPOCHREALTIME` around `zsh -i -c exit` × N (total), an `.zshenv` vs `.zshrc` split, and a `zprof` self-time table. The zprof run is `zsh -f` + a manual `source` of the real rc files, because `/etc/zshenv` force-exports `ZDOTDIR` — the obvious temp-`ZDOTDIR` inject, and `-d`/`--no-globalrcs`, lose that fight. zprof also prints per-function callgraph blocks after its summary table (both start with `N)`), so the parser reads only the summary.
- **`nvim.sh`** — `nvim --startuptime` × N; the median run's log read twice, as nvim's own phase timeline and as per-plugin/`require` cost bucketed on the log's *self* column so nested requires don't double-count their parent. One warm-up drops the cold ShaDa/parser hit.
- **`emacs.sh` + `emacs.el`** — batch load of the real config, median total + per-package init+config from `use-package-statistics`. Two traps: `--batch` alone won't load the user init (so `-q -l early-init … -l init`, and `-q` is also the only point early enough to set `use-package-compute-statistics`), and native-comp is *synchronous* under `--batch` — `native-comp-jit-compilation nil` keeps the load to byte-code so an un-cached `.eln` doesn't dwarf the hot path. GUI frame cost is excluded; an eagerly-`require`d package (no `:defer`/`:commands`, e.g. `man`) shows its full load on the path, and `use-package-statistics-time` sums phase timers that can overlap, so a hot package may read above the wall total.
