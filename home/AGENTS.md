# AGENTS.md — the chezmoi source tree

`home/` is the chezmoi source tree (set by `.chezmoiroot`): everything here deploys to `$HOME`. Edits stage until promoted — commit on `main`, then stop (the root `AGENTS.md` carries the apply/promote loop). This doc is the how-to-edit-source detail; it loads when you touch a file under `home/`.

> This `AGENTS.md` and its sibling `CLAUDE.md` are agent docs, **not** dotfiles — `.chezmoiignore` anchors `/AGENTS.md` `/CLAUDE.md` so chezmoi never deploys them to `~/AGENTS.md`. Anchored (leading `/`), because a bare `CLAUDE.md` would match `.claude/CLAUDE.md` at any depth and silently stop the user `CLAUDE.md` from deploying.

## chezmoi naming conventions

chezmoi encodes each target's attributes in the source filename:
- `dot_foo` → `.foo`. `executable_foo` → `foo` with the `+x` bit — chezmoi ignores the source file's own mode, so the bit *must* be in the name.
- `symlink_foo.tmpl` → a symlink named `foo` whose rendered content is the link target.
- `modify_foo.tmpl` → a script handed the current target on stdin that emits the new content on stdout (used for `settings.json`).
- `run_once_*` / `run_onchange_*` (in `.chezmoiscripts/`) → setup scripts; `_before_`/`_after_` order them around file application. `run_once_` runs once per content hash; `run_onchange_` re-runs whenever an embedded hash comment changes.
- `.tmpl` → Go-template rendered with `.chezmoi.*` facts (`os`, `homeDir`, `sourceDir`).

### The `exact_` caveat
chezmoi only deletes a deployed file when its source disappears *if* the containing dir is marked `exact_`. This repo uses **no `exact_`** dirs, so deletions don't auto-propagate. To remove a stale deployed file, `rm` it (chezmoi won't recreate it). (stow's `--no-folding` pruned on restow; this is the one behavioural difference to keep in mind.)

### OS gating
`.chezmoiignore` is a template: on `darwin` it ignores the Linux-only window-manager configs (`i3`, `X11`, `xmonad`, `~/.xmonad`, and the niri stack `.config/{niri,xdg-desktop-portal,systemd}`). One file, evaluated per machine.

## The claude / cursor config

Deployed Claude config lives under `home/dot_claude/` as ordinary chezmoi files: `USER_CLAUDE.md` (the user instruction file; `symlink_CLAUDE.md` points `~/.claude/CLAUDE.md` at it, so the source tree never holds a literal `CLAUDE.md` for a harness to misread as directory-level instructions), `USER_INSTRUCTION.md`, and the `skills/ agents/ hooks/ rules/` trees. The `+x` bit on `hooks/executable_*.sh` rides in the name. Dev-only tooling — `plugins/`, `tests/`, `rubric.*`, and the `settings.json` merge source — stays in the repo-root `claude/` tree (→ `claude/AGENTS.md`).

No `exact_` dirs, so managed entries coexist with local-only ones (`~/.claude/skills/<local>`, extra `rules/`): chezmoi only ever touches what it manages. **Adding a managed entry:** drop the file under `home/dot_claude/<dir>/`, commit on `main`, and promote.

`~/.cursor/skills/<name>` symlinks to the *deployed* `~/.claude/skills/<name>` (via `{{ .chezmoi.homeDir }}`), so Cursor and Claude share skills.

### `settings.json` — the `modify_` merge
`~/.claude/settings.json` is a *live* file the harness writes to. `home/dot_claude/modify_settings.json.tmpl` is handed the current file on stdin, jq-merges in `.hooks`/`.permissions`/`.env` from `claude/settings.json`, strips `mcpServers`/`statusLine`, force-sets `showThinkingSummaries: true`, and preserves every other (harness-written) key. It runs on every apply and is idempotent. `~/.claude/settings.local.json` is never managed or referenced.

## Externals

`.chezmoiexternal.toml` materializes dependencies on apply:
- **antidote** (zsh plugin manager) — an `archive` external pinned to a release tag (`refreshPeriod = "0"`: fetch once, never silently track a branch). Replaces the old git submodule.
- **tpm** (tmux plugin manager) — a `git-repo` external (`refreshPeriod = "168h"`). Replaces the old `git clone` in a configure hook.

## Per-host data (the git email)

`dot_config/git/config.tmpl` renders `email = {{ .email }}` rather than hardcoding it. `.email` resolves through two layers:
- **`.chezmoidata.toml`** commits the default — the GitHub no-reply (`103758+jaelsasser@users.noreply.github.com`), shared and lowest precedence. chezmoi errors hard on a missing key, so this guarantees `.email` always resolves (un-prompted hosts and the test harness, which applies without `init`).
- **`.chezmoi.toml.tmpl`** is the init-time config template: on `chezmoi init`, `promptStringOnce` asks for the git email and writes it into the machine-local `~/.config/chezmoi/chezmoi.toml` `[data]`, which **outranks** the default. It reads its prior answer back, so re-running `init` never re-prompts.

To set a non-default email on an already-migrated host, re-run `chezmoi init` (regenerates the config, prompts) or hand-add `[data]`\n`email = "…"` to `~/.config/chezmoi/chezmoi.toml`. Apply reads that config automatically — no `--config` needed off the test bench.

## Setup scripts (`.chezmoiscripts/`)

- `run_once_before_etc-zshenv.sh` / `run_once_before_etc-bashrc.sh` — inject the XDG `ZDOTDIR` / bashrc-source line into the system rc (sudo, with a `$HOME` fallback if that's refused).
- `run_once_after_xdg-dirs.sh` — create XDG cache dirs tools expect to exist.
- `run_once_after_emacs-venv.sh` — emacs lisp dir + Python venv.
- `run_onchange_after_zsh-antidote.sh.tmpl` — rebundle antidote plugins when `plugins.zsh` changes (hash-keyed comment).
- `run_onchange_after_claude-plugins.sh.tmpl` — register the repo plugin marketplace and install the `cac` + `diat` plugins when the marketplace manifest changes (guarded on `command -v claude`).
- `run_onchange_after_emacs-bootstrap.sh.tmpl` — eagerly elpaca-install + byte-compile the emacs config whenever any `dot_config/emacs/*.el` or `dot_config/emacs/conf/*.el` changes (hash-keyed via `glob`+`include`). Runs `emacs -nw -l install.el` for live progress; TTY-guarded (`[ -t 0 ]`), so a headless apply skips it and lazy first-launch still installs.

Setup scripts must be idempotent — `run_once_`/`run_onchange_` re-run on hash changes; guard mutations with existence checks.

## XDG compliance

`dot_config/sh/xdg.sh` sets every XDG base directory (cache, config, data, state, runtime) and re-points tools that don't honor them natively. New packages target `~/.config/<pkg>` by default — no bare `~/.*` files unless the tool leaves no other option.

## The vim / nvim two tier

`vim/vimrc` is a plugin-free spine valid in plain vim 9.x and sourced verbatim by `nvim/init.lua`, which then layers plugins through the built-in `vim.pack` manager (flash, mini, oil, treesitter, native LSP) — no external manager, no bootstrap. Treesitter is pinned to `master` (auto-installs parsers with a bundled compiler) and gated on a C compiler, so a toolchain-less container degrades to no-highlight rather than erroring.

The spine's readline insert maps are gated `!has('nvim')`: plain vim gets a hand-rolled subset, nvim gets **vim-rsi** (`C-A/B/D/E/F` + `M-b/M-f/M-d` + command-line readline). Both tiers then add the same Emacs reflexes — `<C-K>` kill-to-EOL, `<C-Y>` paste, `<C-G>` abort (≈ `keyboard-quit`, via `<C-\><C-N>`) — so insert feels identical across plain vim, nvim, and the Emacs evil config (whose insert state *is* plain Emacs). nvim completion is on-demand (`<C-Space>`, `autocomplete` off) to match that config's quiet `company` (`idle-delay nil`). The base asymmetry is deliberate — vim-rsi only loads where a plugin manager exists.

`nvim` migrated from `init.vim` to `init.lua`; the old deployed `~/.config/nvim/init.vim` is inert but lingers (no `exact_`) — `rm` it once. `vim.pack`'s lockfile is runtime state under `~/.config/nvim/` — unmanaged, so chezmoi leaves it alone.

## Theming

**One rule: the terminal owns the palette; only GUI frames theme themselves.** Ghostty and Alacritty set the 16-colour ANSI palette — Ghostty auto-switches light/dark, Alacritty is dark-only. Inside a terminal, **neither nvim nor Emacs loads a colour theme**: they inherit those ANSI colours (nvim runs `termguicolors` *off* with no colorscheme; terminal Emacs stays bare). Only the GUI frames truecolor-theme themselves — **Neovide** loads a `vim.pack` colorscheme that tracks `&background`, **Emacs GUI frames** load a theme following the macOS system appearance. `my-theme.el` holds a switchable rack of light/dark pairs: `M-x my/load-theme-pair` switches the active pair at runtime (lazily elpaca-installing the pick), `C-c t` flips light↔dark. The absent terminal colorscheme and `termguicolors` are the discipline, not an oversight — don't "fix" them.
