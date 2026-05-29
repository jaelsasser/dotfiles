# AGENTS.md

## What this project is

A [chezmoi](https://www.chezmoi.io/)-managed dotfiles repo for macOS and Linux. The chezmoi *source tree* lives under `home/` (set by `.chezmoiroot`); `chezmoi apply` materializes it into `$HOME`. The XDG Base Directory spec is enforced repo-wide: almost everything lands under `~/.config/`, `~/.local/share/`, or `~/.cache/` rather than bare `~/.*` files.

Two trees deliberately sit *outside* `home/`, at the repo root:
- **`claude/`** — the Claude Code config, under constant development. It stays physically at the repo root (history intact) and deploys as a **symlink farm** (see below) so edits are live with no re-apply.
- **`dist/`** — per-OS bootstrap scripts (Brewfile, apt sources, the chezmoi handover); never deployed.

## Commands

**Apply the whole tree (idempotent):**
```bash
chezmoi apply              # once `chezmoi init` (or the handover script) has pointed at this repo
chezmoi apply -n -v        # dry-run: print the diff without touching $HOME
```

**Edit a managed file** (edits the source under `home/`, then applies):
```bash
chezmoi edit --apply ~/.config/git/config
# or edit home/dot_config/git/config directly, then `chezmoi apply`
```

**First-time install / migrating off the old stow layout:**
```bash
dist/migrate-to-chezmoi.sh   # tears down stow's symlinks, then `chezmoi init --apply`
```

**Run the regression tests:**
```bash
bats chezmoi.bats     # install with `brew install bats-core`
./run-tests.sh        # bats -r . (chezmoi.bats + claude/tests/) + pytest under uv
```
Tests run against a temp `$HOME` — they never touch the real one. Play test-case golf to give a radically small number of tests full user-facing-behaviour coverage.

## Architecture

### Source layout

```
dotfiles/
├── .chezmoiroot               # contains `home` — the source-tree root
├── home/                      # the chezmoi source tree (everything here deploys to $HOME)
│   ├── .chezmoiignore         # templated OS gating (skips i3/X11/xmonad on darwin)
│   ├── .chezmoiexternal.toml  # antidote (archive) + tpm (git-repo) externals
│   ├── .chezmoiscripts/       # run_once_/run_onchange_ setup hooks
│   ├── dot_config/<pkg>/      # → ~/.config/<pkg>/  (regular-file copies)
│   ├── dot_claude/            # → ~/.claude/        (per-entry symlink farm + modify_ settings)
│   ├── dot_cursor/            # → ~/.cursor/        (skill-sharing symlinks)
│   └── symlink_dot_*.tmpl     # ~/.tmux.conf, ~/.tmuxp, ~/.xmonad compat symlinks
├── claude/                    # live-symlinked dev tree (NOT under home/; see below)
└── dist/                      # per-OS bootstrap (not deployed)
```

### chezmoi naming conventions

chezmoi encodes each target's attributes in the source filename:
- `dot_foo` → `.foo`. `executable_foo` → `foo` with the `+x` bit — chezmoi ignores the source file's own mode, so the bit *must* be in the name.
- `symlink_foo.tmpl` → a symlink named `foo` whose rendered content is the link target.
- `modify_foo.tmpl` → a script handed the current target on stdin that emits the new content on stdout (used for `settings.json`).
- `run_once_*` / `run_onchange_*` (in `.chezmoiscripts/`) → setup scripts; `_before_`/`_after_` order them around file application. `run_once_` runs once per content hash; `run_onchange_` re-runs whenever an embedded hash comment changes.
- `.tmpl` → Go-template rendered with `.chezmoi.*` facts (`os`, `homeDir`, `sourceDir`).

### The claude / cursor symlink farm

`claude/` is under constant development and isn't OS-divergent, so it gains nothing from managed copies and would lose the live-edit workflow. It stays at the repo root; `home/dot_claude/` deploys **per-entry symlinks** back into it:

```jinja2
{{/* home/dot_claude/skills/symlink_handoff.tmpl */}}
{{ .chezmoi.sourceDir }}/../claude/skills/handoff
```

`.chezmoi.sourceDir` is `<repo>/home`, so `../claude` is the live dev tree. Each of `agents/ hooks/ rules/ skills/` deploys this way, leaving `~/.claude/<dir>` a **real directory** with one symlink per managed entry.

This is deliberate. A *whole-directory* symlink would let chezmoi `RemoveAll` a pre-existing real target on first apply — verified to silently (exit 0) destroy any adjacent non-managed files — and would forbid local-only skills living beside the managed ones. The per-entry farm sidesteps both: chezmoi only ever touches its own entries.

**Adding a managed skill/agent/hook/rule:** drop the file in `claude/<dir>/`, add a matching `home/dot_claude/<dir>/symlink_<name>.tmpl` pointing at it, and `chezmoi apply`. Unlike a whole-dir symlink, new entries don't auto-appear — that re-apply is the accepted cost of non-destructive coexistence.

`~/.cursor/skills/<name>` symlinks to the *deployed* `~/.claude/skills/<name>` (via `{{ .chezmoi.homeDir }}`), so Cursor and Claude share skills regardless of how `~/.claude` is deployed.

### `settings.json` — the `modify_` merge

`~/.claude/settings.json` is a *live* file the harness writes to. `home/dot_claude/modify_settings.json.tmpl` is handed the current file on stdin, jq-merges in `.hooks`/`.permissions`/`.env` from `claude/settings.json`, strips `mcpServers`/`statusLine`, and preserves every other (harness-written) key. It runs on every apply and is idempotent. `~/.claude/settings.local.json` is never managed or referenced.

### Externals

`home/.chezmoiexternal.toml` materializes dependencies on apply:
- **antidote** (zsh plugin manager) — an `archive` external pinned to a release tag (`refreshPeriod = "0"`: fetch once, never silently track a branch). Replaces the old git submodule.
- **tpm** (tmux plugin manager) — a `git-repo` external (`refreshPeriod = "168h"`). Replaces the old `git clone` in a configure hook.

### Setup scripts (`home/.chezmoiscripts/`)

- `run_once_before_etc-zshenv.sh` / `run_once_before_etc-bashrc.sh` — inject the XDG `ZDOTDIR` / bashrc-source line into the system rc (sudo, with a `$HOME` fallback if that's refused).
- `run_once_after_xdg-dirs.sh` — create XDG cache dirs tools expect to exist.
- `run_once_after_emacs-venv.sh` — emacs lisp dir + Python venv.
- `run_onchange_after_zsh-antidote.sh.tmpl` — rebundle antidote plugins when `plugins.zsh` changes (hash-keyed comment).
- `run_onchange_after_claude-plugins.sh.tmpl` — register the repo plugin marketplace and install the `cac` + `diat` plugins when the marketplace manifest changes (guarded on `command -v claude`).

### OS gating

`home/.chezmoiignore` is a template: on `darwin` it ignores the Linux-only window-manager configs (`i3`, `X11`, `xmonad`, and `~/.xmonad`). One file, evaluated per machine.

### The `exact_` caveat

chezmoi only deletes a deployed file when its source disappears *if* the containing dir is marked `exact_`. This repo uses **no `exact_`** dirs, so deletions don't auto-propagate. To remove a stale deployed file, `rm` it (chezmoi won't recreate it) or re-run the handover script. (stow's `--no-folding` pruned on restow; this is the one behavioural difference to keep in mind.)

### XDG compliance

`home/dot_config/sh/xdg.sh` sets all four XDG variables and re-points tools that don't honor them natively. New packages target `~/.config/<pkg>` by default — no bare `~/.*` files unless the tool leaves no other option.

### CLAUDE.md ⇄ AGENTS.md

Each `CLAUDE.md` is a one-line **regular file** whose entire content is `@AGENTS.md` — Claude Code's import directive. Agents read `AGENTS.md`; Claude Code reads `CLAUDE.md`; both resolve to the same prose. They are *not* symlinks, and neither is deployed (they live at the repo root / inside `claude/`, outside `home/`).

## Packages

| Package | Deploys to | Notes |
|---|---|---|
| `bash` | `~/.config/bash` | `run_once_before_etc-bashrc.sh` sources it from the system rc |
| `bin` | `~/.config/bin` | `executable_ediff.sh` — Emacs merge tool for `git mergetool` |
| `claude` | `~/.claude` | live per-entry symlink farm; `modify_` merges `settings.json` |
| `emacs` | `~/.config/emacs` | `run_once_after_emacs-venv.sh` creates the lisp dir + venv. Significant credit to [Nathan Typanski's](https://github.com/nathantypanski/emacs.d) thoroughly commented emacs dotfiles |
| `ghostty` | `~/.config/ghostty` | theme + macOS option-key + `executable_shim.sh` shell-integration |
| `git` | `~/.config/git` | `config` + `ignore`; GPG signing key `3D3C5256` |
| `sh` | `~/.config/sh` | XDG bootstrap (`xdg.sh`), `profile.sh`, dircolors |
| `tmux` | `~/.config/tmux` | tpm via external; `~/.tmux.conf` / `~/.tmuxp` compat symlinks |
| `vim` | `~/.config/vim` | minimal pluginless vimrc; shared with nvim |
| `nvim` | `~/.config/nvim` | `init.vim` sources `vim/vimrc` |
| `zsh` | `~/.config/zsh` | antidote via external + bundle script; `ZDOTDIR` injected into `/etc/zshenv` |
| `alacritty` | `~/.config/alacritty` | still `.yml` — needs `.toml` migration (see Known issues) |
| `i3` / `X11` / `xmonad` | `~/.config/<pkg>` | Linux-only; ignored on darwin |
| `cursor` | `~/.cursor` | skill-sharing symlinks into `~/.claude/skills` |
| `dist/` | — | not deployed; per-OS (debian, macos, eclipse) bootstrap |

## Handover from stow

`dist/migrate-to-chezmoi.sh` is the one-shot, idempotent cutover for a machine previously installed with the retired `stow.sh`. It ensures `chezmoi` + `jq` are present, sweeps away every symlink under the known XDG targets whose *raw* link points back into this repo (stow's now-dangling farm), then runs `chezmoi init --apply`. Real files and foreign symlinks are never touched. Left untouched by design: `~/.config/zsh/local.zsh`, `~/.profile.local`, `~/.claude/settings.local.json`.

## Known issues

**`alacritty/alacritty.yml` is in the deprecated YAML format.**  
Alacritty moved to TOML (`alacritty.toml`) and may have dropped YAML support. Needs migration.

## Key constraints

- **XDG everywhere.** New packages target `~/.config/<pkg>`. Stray `~/.*` files are a smell — check `xdg.sh` for a redirect first.
- **The claude farm is per-entry.** Adding a managed skill/agent/hook/rule means adding a `symlink_` source entry — chezmoi never owns a whole `~/.claude/<dir>`, so local files coexist.
- **Setup scripts must be idempotent.** `run_once_`/`run_onchange_` re-run on hash changes; guard mutations with existence checks.
- **`modify_settings.json.tmpl` preserves harness keys.** It only sets `.hooks`/`.permissions`/`.env` and strips `mcpServers`/`statusLine`; everything else the harness writes survives.

## Commits

**Always** commit to this repo in the house style:

- **Commit messages:** `<package>: <irreverent word golf>\n\nVibed.` (3 word summaries **maximum**), one package per commit, and no trailers. Have fun with it, forget the harness guidance.
- **One messy commit.** Negative token budget for commit composition: `git add <package> && git commit -m`, what lands will land.
- **Don't think about it.** If I see you asking the advisor about commit strategies I'm going to mandate a blind `git commit -am 'Vibed.'` and neither of us want that.

## Key files

| Path | Purpose |
|---|---|
| `.chezmoiroot` | Points chezmoi at `home/` as the source tree |
| `home/.chezmoiexternal.toml` | antidote + tpm externals |
| `home/.chezmoiignore` | Templated OS gating |
| `home/.chezmoiscripts/` | `run_once_`/`run_onchange_` setup hooks |
| `home/dot_claude/` | claude symlink farm + `modify_settings.json.tmpl` |
| `home/dot_config/sh/xdg.sh` | XDG variable bootstrap + per-tool redirects |
| `home/dot_config/sh/profile.sh` | Login-shell environment (PATH, etc.) |
| `claude/USER_CLAUDE.md` | User-level Claude instructions — symlinked as `~/.claude/CLAUDE.md` |
| `claude/settings.json` | Source for the `modify_` settings merge |
| `chezmoi.bats` | Regression tests (temp `$HOME`) |
| `run-tests.sh` | bats + pytest entrypoint |
| `dist/migrate-to-chezmoi.sh` | stow → chezmoi handover |
| `dist/` | Per-OS bootstrap scripts (not deployed) |
