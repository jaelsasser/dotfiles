# AGENTS.md

GNU Emacs config — **emacs-plus** on macOS (NS port), also valid on Linux/Windows. elpaca + `use-package` (`use-package-always-ensure t`), XDG-aware (`user-emacs-data` under `$XDG_DATA_HOME/emacs`). Theme/font/evil split into `conf/`.

## Daemon-first — why the frame hooks aren't ceremony

Emacs runs as a **systemd user service** (the daemon); frames are opened by `emacsclient`. At daemon startup there is **no graphical frame**, so anything that queries the display returns nil or garbage: `font-family-list` sees the tty's families, `display-graphic-p` is nil, `ns-system-appearance` is unbound. Frame-dependent setup therefore **defers to `after-make-frame-functions`** — fire on the first GUI client, then `remove-hook` itself — *and* runs inline for a non-daemon (direct GUI) launch.

`conf-font.el` and `conf-theme.el` both do this dance. It is load-bearing: strip the hook and the daemon's first `emacsclient` frame gets no font / no theme. Don't "simplify" it away.

Corollary (the repo-wide theming rule): **tty clients stay bare** — terminal frames inherit the ANSI palette, only GUI frames theme/font themselves. `conf-theme` guards on `display-graphic-p` so a terminal client never loads a colour theme.

## The symlink farm (deployment)

This `emacs/` tree sits at the **repo root, not under `home/`** — a per-entry symlink farm, exactly like `claude/`. `home/dot_config/emacs/symlink_*.tmpl` resolves each file into the source tree's sibling `emacs/` and deploys it to `~/.config/emacs`.

**Adding a file** (a new `conf/conf-*.el`, or a top-level one) means adding a matching `home/dot_config/emacs/<...>/symlink_<name>.el.tmpl`, then promote on `main` (`task sideload` → `chezmoi apply`). New files **don't auto-appear** — that's the cost of the non-destructive farm. `AGENTS.md` / `CLAUDE.md` here have no `symlink_` entry, so they're repo-only, never deployed.

## Layout

- `early-init.el` — pre-frame: GC tuning, frame chrome (`default-frame-alist`), and the macOS PATH seat so native-comp's libgccjit finds Homebrew gcc before any `.eln` builds.
- `init.el` — the bulk: defaults, keybinds, then `(require 'conf-theme)` → `conf-font` → `conf-evil`, then packages. `M-x my/byte-compile-config` recompiles `init.el` + the `conf/` tree.
- `conf/bootstrap.el` — elpaca install + `use-package` wiring; required first.
- `conf/conf-theme.el` — lazily-installed light/dark theme rack (`setopt my-theme-pair` to switch, `C-c t` to flip), following `ns-system-appearance`.
- `conf/conf-font.el` — global font with graceful fallback + editor-wide ligatures.
- `conf/conf-evil.el` — evil config.

`run_once_after_emacs-venv.sh` (a chezmoi script, not here) creates the XDG lisp dir + Python venv before first launch.
