# AGENTS.md

GNU Emacs config — **emacs-plus** on macOS (NS port), also valid on Linux/Windows. elpaca + `use-package` (`use-package-always-ensure t`), XDG-strict via `no-littering` (see Path hygiene). Theme/font/evil split into `conf/`.

## Daemon-first — why the frame hooks aren't ceremony

Emacs runs as a **systemd user service** (the daemon); frames are opened by `emacsclient`. At daemon startup there is **no graphical frame**, so anything that queries the display returns nil or garbage: `font-family-list` sees the tty's families, `display-graphic-p` is nil, `ns-system-appearance` is unbound. Frame-dependent setup therefore **defers to `after-make-frame-functions`** — fire on the first GUI client, then `remove-hook` itself — *and* runs inline for a non-daemon (direct GUI) launch.

`my-font.el` and `my-theme.el` both do this dance. It is load-bearing: strip the hook and the daemon's first `emacsclient` frame gets no font / no theme. Don't "simplify" it away.

Corollary (the repo-wide theming rule): **tty clients stay bare** — terminal frames inherit the ANSI palette, only GUI frames theme/font themselves. `my-theme` guards on `display-graphic-p` so a terminal client never loads a colour theme.

## Path hygiene — why no-littering loads first

Nothing but config belongs in the config dir, so package state is forced into XDG `data`/`cache`. `no-littering` themes every package's paths to `$XDG_DATA_HOME/emacs/{var,etc}` (`user-emacs-data`) — but only for packages that load *after* it, so it's the **first** package: installed and `elpaca-wait`ed in `bootstrap.el`, ahead of init.el's rack. `no-littering-theme-backups` owns the backup/auto-save paths; init.el keeps only the policy (`version-control`, `delete-old-versions`, …).

Two things no-littering can't reach, set by hand:
- **elpaca's own dirs** — it bootstraps before no-littering exists. `bootstrap.el` splits them: sources (durable clones) → `user-emacs-data`, builds + cache (regenerable) → `user-emacs-cache` (`$XDG_CACHE_HOME/emacs`).
- **native-comp eln-cache** — redirected before the first `.eln` builds, so `startup-redirect-eln-cache` → `user-emacs-cache` lives in `early-init.el`, not here.

## Deployment

This config deploys as ordinary chezmoi files under `home/dot_config/emacs/` → `~/.config/emacs`, like any other package. Adding or renaming a `.el` is just adding the file and promoting (`task sideload` → `chezmoi apply`) — no farm to regenerate. The sibling `AGENTS.md`/`CLAUDE.md` are `.chezmoiignore`d; `emacs.bats` lives at the repo root (`task test:emacs`).

## Layout

- `early-init.el` — pre-frame: GC tuning, frame chrome (`default-frame-alist`), the eln-cache redirect (Path hygiene), and the macOS PATH seat so native-comp's libgccjit finds Homebrew gcc before any `.eln` builds.
- `init.el` — the bulk: defaults, keybinds, then `(require 'my-theme)` → `my-font` → `my-evil`, then packages.
- `install.el` — eager elpaca-install + byte-compile driver, loaded *after* `init.el` (never at startup). The `run_onchange_after_emacs-bootstrap` chezmoi script runs it in a `-nw` frame on every emacs-source change (live progress); `emacs.bats` runs it under `--batch` where `noninteractive` makes warnings fatal.
- `bootstrap.el` — elpaca install + `use-package` wiring, then `no-littering`; relocates elpaca's dirs (Path hygiene). Required first.
- `conf/my-theme.el` — lazily-installed light/dark theme rack (`M-x my/load-theme-pair` to switch, `C-c t` to flip), following `ns-system-appearance`.
- `conf/my-font.el` — global font with graceful fallback + editor-wide ligatures.
- `conf/my-evil.el` — evil config.
- `conf/my-agent.el` — Claude Code + Monet IDE bridge on the ghostel (libghostty) terminal backend.

`run_once_after_emacs-venv.sh` (a chezmoi script, not here) creates the XDG lisp dir + Python venv before first launch.
